#!/bin/bash
find source \( -name "*.ppu" -or -name "*.ppl" -or -name "*.o" -or -name "*.or" \) -exec rm {} \;
