#!/bin/bash
OLD_PATH=$PWD
while [ ! -e ".OURCRAFT_MAIN_DIRECTORY_DO_NOT_DELETE_THIS_FILE" ];
do
    cd ..
done

echo "Searching for environment..."

TEMP_FILE="/dev/shm/tempOurcraftUnits"
find "source/Environment" -maxdepth 1 -name "*.pas" | cut -f 3 -d "/" | cut -f 1 -d "." > $TEMP_FILE

echo "Found $( cat $TEMP_FILE | wc -l ) elements:"
cat $TEMP_FILE

awk '{ printf $1; printf ", "  }' $TEMP_FILE | head -c -2 > "source/Preprocesor/EnvironmentUnits.inc"
awk '{ printf $1; printf ".RegisterElementCreator(Self, @RegisterCreator); "  }' $TEMP_FILE > "source/Preprocesor/EnvironmentRegister.inc"

echo "Attributes"

find "source/Environment" -name "*.attribute" -exec cat {} \; | sort | tee $TEMP_FILE | \
    awk 'BEGIN { 
            FS="="
            Out=""
        }

        { 
            Out=Out "(ID:" NR-1 "; Name:\x27" $1  "\x27; DefaultValue:PtrUInt(" $2 ")), \n" 
        }  

        END {
            printf "AttributeList : array[0.." NR-1 "] of TEnvironmentElementAttribute = ( \n" Out
        }' | head -c -3 > "source/Preprocesor/AttributeList.inc"
echo ");" >> "source/Preprocesor/AttributeList.inc"
    
rm $TEMP_FILE
cd $OLD_PATH
