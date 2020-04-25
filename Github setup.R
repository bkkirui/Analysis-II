library(usethis)
?use_github
edit_r_environ() ##TOKEN = 'c2a1e2087e5b8dd5aeab9bddf81844f1cf8b05f2' obtained from https://github.com/settings/tokens
library(usethis)
use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))

