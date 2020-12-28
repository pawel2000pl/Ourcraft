#!/bin/bash
find . \( -name "*.pas" -or -name "*.lpr" -or -name "*.sh" -or -name "*.inc" \) -and \( -not -path "*backup*" \)  -and \( -not -path "*?opia*" \) -and \( -not -path "*lib*" \) -exec cat {} \; 
