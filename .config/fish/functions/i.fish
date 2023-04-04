function i --description 'auto npm/yarn/pnpm install'
  if test -e package-lock.json
    echo "Found package-lock.json. Running `npm install`"
    npm install
  else if test -e yarn.lock
    echo "Found yarn.lock. Running `yarn install`"
    yarn install
  else if test -e pnpm-lock.yaml
    echo "Found pnpm-lock.yaml. Running `pnpm install`"
    pnpm install
  else
    echo "No lockfile found. Choose package manager:"
    set -l tools npm yarn pnpm
    set -l choice (string join \n $tools | fzf)
    if test -n "$choice"
      $choice install
    end
  end
end
