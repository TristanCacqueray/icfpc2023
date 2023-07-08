# Fetch a problem:

```ShellSession
PB=18
curl https://api.icfpcontest.com/problem?problem_id=$PB | jq '.Success | fromjson' -cr > ./problems/$PB-problem.json
```
