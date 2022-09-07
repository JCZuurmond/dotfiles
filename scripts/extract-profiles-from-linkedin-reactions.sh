#!/bin/bash
# Requires htmlq: https://github.com/mgdm/htmlq

# File should be a html file containing the profiles in the reaction list
# for example under a Linked post.
# In a browser: "inspect element" -> find element with profiles list -> copy to html file
FILE=$1
OUTPUT_FILE=$2

cat $FILE | htmlq --attribute href a > profiles_$OUTPUT_FILE

# filter title | remove the "View ... profile" lines | remove white lines | cut name
# using rev to find last name
cat $FILE | htmlq '.artdeco-entity-lockup__title' --text -w | grep -v 'View' | grep -v -e '^$' | cut -d ' ' -f1 > first_names_$OUTPUT_FILE
cat $FILE | htmlq '.artdeco-entity-lockup__title' --text -w | grep -v 'View' | grep -v -e '^$' | rev | cut -d ' ' -f1 | rev > last_names_$OUTPUT_FILE

echo 'Profile,First Name,Last Name' > $OUTPUT_FILE
paste -d, profiles_$OUPUT_FILE first_names_$OUTPUT_FILE last_names_$OUTPUT_FILE >> $OUTPUT_FILE
