@ECHO OFF
Rscript "deploy.R"
git add --all && git commit -m 'WIP' && git push origin master
PAUSE
