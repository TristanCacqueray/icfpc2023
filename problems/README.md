# Fix the formating:

```ShellSession
echo -e $(cat ~/Downloads/icfp/problem-1.json) | sed -e 's/\\"/"/g'  -e 's/^"//' -e 's/"$//' | jq -cr > problem-1.json
```
