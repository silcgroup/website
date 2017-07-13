REF=`git rev-parse HEAD`
BRANCH=`git branch`

git stash

./site.hs build
rsync -a --filter='P _site/'      \
      --filter='P _cache/'     \
      --filter='P .git/'       \
      --filter='P .gitignore'  \
      --filter='P .stack-work' \
      --delete-excluded        \
      _site/ .
git checkout deploy
git add -A
git commit -m "Deploy $REV"
git push deploy deploy:master
git checkout $BRANCH
git branch -D deploy

git stash pop
