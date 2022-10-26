
## 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

.libPaths("C:/R/win-library/")
install.packages("usethis")
library(usethis)
use_git_config(user.name="waltercervi", user.email="walter.rossicervi@wur.nl") 
git_sitrep()
usethis::create_github_token()
install.packages("gitcreds") 
library(gitcreds)
library(devtools)
gitcreds::gitcreds_set()
gitcreds::gitcreds_get()
usethis::use_git()
usethis::use_github()
