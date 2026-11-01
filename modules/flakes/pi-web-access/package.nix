{
  lib,
  buildNpmPackage,
  fetchurl,
  nodejs_24,
}:

buildNpmPackage {
  pname = "pi-web-access";
  version = "0.46.0";

  src = fetchurl {
    url = "https://registry.npmjs.org/pi-web-access/-/pi-web-access-0.46.0.tgz";
    hash = "sha256-FPnsntJ21UEYmWeI8mjlNwcKrAXV8KOpMzhpaY1alK0=";
  };

  nodejs = nodejs_24;
  npmDepsHash = "sha256-T+BgivXo70stWOgu9E7UUG04sqe/Bfhg3/birTJprcQ=";
  dontNpmBuild = true;

  # Use the published bundle, with a lockfile containing only runtime deps.
  # Pi resolves its own host modules; installing copies would duplicate them.
  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    cp -r dist node_modules package.json CHANGELOG.md LICENSE "$out/"
    # Conventional entry point for ~/.pi/agent/extensions/<name>/ discovery.
    ln -s dist/index.js "$out/index.js"
    runHook postInstall
  '';

  meta = {
    description = "Web search and content extraction extension for Pi";
    homepage = "https://github.com/nicobailon/pi-web-access";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
