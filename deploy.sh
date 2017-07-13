REF=`git rev-parse HEAD`
BRANCH=`git rev-parse --abbrev-ref HEAD`

git branch -D autodeploy
git checkout -b autodeploy

./site.hs build
rsync -a --filter='P _site/'      \
      --filter='P _cache/'     \
      --filter='P .git/'       \
      --filter='P .gitignore'  \
      --filter='P .stack-work' \
      --delete-excluded        \
      _site/ .

git reset --soft deploy/master
git add -A
git commit -m "Deploy silcgroup/website@$REF"

git push deploy autodeploy:master

git checkout $BRANCH
git branch -D autodeploy
