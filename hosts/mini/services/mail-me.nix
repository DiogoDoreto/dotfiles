{ pkgs, ... }:

# mailme - A simple email queue system
#
# Goal: Provide a CLI tool to send emails to yourself via SMTP.
#
# How it works:
# 1. User runs: mailme "Subject" "Body"
# 2. CLI writes a file to /var/spool/mailme with subject (line 1) and body (rest)
# 3. systemd path watcher detects the new file and triggers the worker service
# 4. Worker reads the file, formats it as an email with headers, sends via SMTP, and deletes it
# 5. Emails are sent using curl over SMTPS (port 465) to Fastmail
#
# Components:
# - mailme (CLI): User-facing command to queue emails
# - mailme-worker (service): Background worker that processes the queue
# - mailme-watcher (path): systemd path unit that triggers the worker
# - /var/spool/mailme: Queue directory (writable by all users)

let
  spoolDir = "/var/spool/mailme";
  myEmail = "diogo@doreto.com.br";

  # 1. The Worker Script
  # It looks for files, formats email headers, sends email, and deletes the file.
  mailWorker = pkgs.writeShellScriptBin "mailme-worker" ''
    SPOOL_DIR="${spoolDir}"
    SMTP_PASS=$(cat "$CREDENTIALS_DIRECTORY/smtp_pass")

    # Loop through files in the spool directory
    for file in "$SPOOL_DIR"/*; do
      # Check if file exists to avoid errors on empty runs
      [ -e "$file" ] || continue
      echo "Processing $file ..."

      # Read the subject from the first line
      SUBJECT=$(head -n 1 "$file")
      
      # Read the body from the rest of the file
      BODY=$(tail -n +2 "$file")

      # Format and send the email with headers
      {
        echo "From: $MAIL_FROM"
        echo "To: $MAIL_FROM"
        echo "Subject: $SUBJECT"
        echo ""
        echo "$BODY"
      } | ${pkgs.curl}/bin/curl --url "$SMTP_URL" \
        --ssl-reqd \
        --mail-from "$MAIL_FROM" \
        --mail-rcpt "$MAIL_FROM" \
        --user "$SMTP_USER:$SMTP_PASS" \
        --upload-file - \
        --connect-timeout 30 \
        -s -S

      echo "Processed and removed $file"
      rm "$file"
    done
  '';

  # 2. The User CLI (Runs as User)
  # It writes the subject and body to a file in the spool dir.
  mailCli = pkgs.writeShellScriptBin "mailme" ''
    SUBJECT="$1"
    BODY="$2"

    if [[ -z "$BODY" ]]; then
        echo "Usage: mailme <subject> <body>"
        exit 1
    fi

    # Create a unique temporary filename
    SPOOL_DIR="${spoolDir}"
    FILENAME="msg-$(date +%s)-$RANDOM"
    TEMP_FILE="/tmp/$FILENAME.tmp"
    FINAL_FILE="$SPOOL_DIR/$FILENAME"

    # Write subject as first line, body as rest
    echo "$SUBJECT" > "$TEMP_FILE"
    echo "$BODY" >> "$TEMP_FILE"

    # Atomic move to trigger the watcher
    mv "$TEMP_FILE" "$FINAL_FILE"
    echo "Email queued."
  '';

in
{
  environment.systemPackages = [ mailCli ];

  users.users.mailme = {
    isSystemUser = true;
    group = "mailme";
  };
  users.groups.mailme = { };

  # Create the Spool Directory
  # mailme user owns it, but everyone can write to it
  systemd.tmpfiles.rules = [
    "d ${spoolDir} 0777 mailme mailme - -"
  ];

  systemd.services.mailme-worker = {
    description = "Process outgoing email queue";
    serviceConfig = {
      Type = "oneshot";
      User = "mailme";
      Group = "mailme";

      Environment = [
        "SMTP_URL=smtps://smtp.fastmail.com:465"
        "MAIL_FROM=${myEmail}"
        "SMTP_USER=${myEmail}"
      ];
      LoadCredential = "smtp_pass:/etc/secrets/fastmail";
    };
    script = "${mailWorker}/bin/mailme-worker";
  };

  systemd.paths.mailme-watcher = {
    description = "Watch for new emails in spool";
    wantedBy = [ "multi-user.target" ];
    pathConfig = {
      DirectoryNotEmpty = spoolDir;
      Unit = "mailme-worker.service";
    };
  };
}
