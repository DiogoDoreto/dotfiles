# Windows initialization

- Search windows menu for "developer mode" and enable it (in system settings -> privacy & security -> for developers)

## SSH

```powershell
Get-Service ssh-agent | Set-Service -StartupType Automatic -PassThru | Start-Service
start-ssh-agent.cmd

ssh-keygen -t ed25519 -C "github"
ssh-add .ssh/id_ed25519
git config --global core.sshCommand C:/Windows/System32/OpenSSH/ssh.exe
type .ssh/id_ed25519.pub
```

## winget

```powershell
winget install starship
```

## init dotfiles

```powershell
mkdir .config
git clone --depth 1 git@github.com:AstroNvim/AstroNvim.git $env:LOCALAPPDATA\nvim
git clone git@github.com:DiogoDoreto/dotfiles.git ".config/dotfiles"

git config --global include.path $env:HOMEPATH\.config\dotfiles\.config\git\config

mkdir $PROFILE
rmdir $PROFILE
New-Item -ItemType SymbolicLink -Target $env:HOMEPATH\.config\dotfiles\.config\powershell\Microsoft.PowerShell_profile.ps1 -Path $PROFILE

New-Item -ItemType SymbolicLink -Target $env:HOMEPATH\.config\dotfiles\.config\astronvim\lua\user -Path $env:LOCALAPPDATA\nvim\lua\user

New-Item -ItemType SymbolicLink -Target $env:HOMEPATH\.config\dotfiles\.config\git\ -Path $env:HOMEPATH\.config\git
New-Item -ItemType SymbolicLink -Target $env:HOMEPATH\.config\dotfiles\.config\wezterm\ -Path $env:HOMEPATH\.config\wezterm
New-Item -ItemType SymbolicLink -Target $env:HOMEPATH\.config\dotfiles\.config\zk\ -Path $env:HOMEPATH\.config\zk
```
