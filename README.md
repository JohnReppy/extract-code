# Extract Code

 A filter for extracting TeX'able code from source files.
 Commands are specified as comments that must begin at the
 start of a line and have "(*@" as their first three characters.

 The supported commands are (using SML comment syntax):

``` sml
(*@FILE attributes files *)
```
open the list of files for extracted output with the
specified attibutes (attributes are @LEFT, @CENTER)

``` sml
(*@BEGIN files *)
```
start directing output to the files

``` sml
(*@END files *)
```
stop directing output to the specified files; if files is empty, then
all active files are stoped.

``` sml
(*@BEGIN-ELLIPSE *)
```(*@END-ELLIPSE *)`
suspend/resume output to the current files.

``` sml
(*@BEGIN-HIGHLIGHT *)
(*@END-HIGHLIGHT *)
```
suspend/resume highlighting the output to the current files.  Currently,
highlighting is represented by underlining.

``` sml
(*@IF-FILE files *)
(*@ELIF-FILE files *)
(*@ELSE *)
(*@END-IF *)
```
conditional extraction

``` sml
(*@INSERT
  ... stuff ...
*)
```
insert the stuff into the open files

``` sml
(*@KW*)token(*@WK*)
```
treat the token as a keyword.  This markup is not supported for all
source languages.
