#!/bin/bash
set -e

tee ~/.netrc > /dev/null <<EOF
machine github.com
login silc-bot
password $GH_TOKEN
EOF

rsync -a --filter='P _site/'      \
      --filter='P _cache/'     \
      --filter='P .git/'       \
      --filter='P .gitignore'  \
      --filter='P .stack-work' \
      --delete-excluded        \
      _site/ .


git remote add deploy https://github.com/silcgroup/silcgroup.github.io.git
git config --global user.email "dbp+silc@dbpmail.net"
git config --global user.name "SILC Bot"
git fetch deploy

REV="** deploy silcgroup/website@$TRAVIS_COMMIT"
git fetch deploy
git reset --soft deploy/master
git add .
git status
git commit -m "$REV"

git fetch origin
CURRENT_HEAD=`git rev-parse origin/master`
if [ "$TRAVIS_COMMIT" = "$CURRENT_HEAD" ]
then
    echo "Committing..."
    git push deploy HEAD:refs/heads/master
else
    echo "Aborting; detected race..."
    exit 1
fi
