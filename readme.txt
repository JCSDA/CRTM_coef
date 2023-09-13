Readme File for develop branch

Development strategy:
(0) All development is based off of the develop branch.  Master will be updated when releases are created.  No direct write to the develop branch will be allowed.
(1) If you want to contribute, please create a feature/initials_branchname branch based off of develop.   For example:  git checkout develop; git pull; git checkout -B feature/btj_AMSUAapc
(2) When you're ready to ask for a merge with develop, first issue git merge develop (from your branch), git commit, git push.   then go to the https://github.com/JCSDA/CRTM_coef page, and click New Pull Request.  
(3) This repo is git-flow compatible, so if you prefer to use git-flow, please do so.  
(4) This repo has git-lfs installed for handling large  / binary files.   The files that are tracked are located in the .gitattributes file, located in the top level.   To add a new binary file or large file type that's not being tracked, use git lfs track "*.nc4" or whatever the extension or other glob is, this will add to the .gitattributes file.  Don't forget to push 


