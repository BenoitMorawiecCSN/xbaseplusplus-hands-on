# Workflow, config, and tools

Replicable workspace configuration to quickstart in a proper environment with xbase++ and windows

Workflow setup and usability

## Vscode

problem: could not access ENV vars

Have a proper terminal
- File > Preferences > Settings 
- terminal > integrated  > default profile
- choose cmd or powershell

## Git

### Multiple line commits 

Good practice: 
- first commit line, title
- next lines, commit description

As bellow:
```
commit -m "Commit Title

This is the commit comment
on multiple lines to explain
the goals of the commit"
```

As it does not work on windows:
- 1. create a file `commit-message.txt` at the root of the directory
- 2. put the multi line commit message there
- 3. commit using a file content as message `git commit -F commit-message.txt`

Or multiple commit lines as arguments in cmd with multiple `-m`
```
git commit -m "first line" -m "second line" -m "third line"
```