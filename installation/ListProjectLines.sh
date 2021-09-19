#!/bin/bash
find . \( -name "*.pas" -or -name "*.lpr" -or -name "*.sh" -or -name "*.inc"  -or -name "*.lpi" -or -name "*.res" -or -name "*.lfm" -or -name "*.lps" -or -name "*.generic" -or -name "*.ico" -or -name ".OURCRAFT_MAIN_DIRECTORY_DO_NOT_DELETE_THIS_FILE" \) -and \( -not -path "*backup*" \)  -and \( -not -path "*?opia*" \) -exec cat {} \; | wc -l
