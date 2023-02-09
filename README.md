## Some design decisions I took
(Note that there are some whitespace characters that are not space or tabspace in the mdtab-2023.mdt file, so please backspace and put the spaces again, or shift+Tab the whole and then tab again )
1. (---) is implemented as a dashed line ONLY when these are the only 3 characters on any line. This was done for more readability and so any random --- does not expand to a horizontal rule. also, no whitespace characters should be present before or after. (after this only the "\n" character must be there)

4. Codeblocks are implemented as according to markdown, so if an indented block follows a paragraph without a blank line in between, it will be considered to be a part of the paragraph instead of a codeblock. Hence a blankline separating codeblock from paragraphs are necessary

5. All escape characters are properly dealt with.

6. Errors and exceptions 
 - \* and \- need to be escaped. If any \* is found that does not correspond to a bold, italic or list form then an **AsterixNotMatched** Error is raised and also printed in the html file at the end.
 - if an open "\[" is found without escaping, then it throws a **bracketNotMatched** error if it doesnt close in the same line.
  in case it is supposed to be a link, then a "(.)" clause should follow right after the "\]" ends, otherwise it will be printed normally.

7. codeblocks works fine for four spaces always BUT they should be 4 or more spaces or 1 or more tabs.In the assignment's .md file, the link "cse.iitd.ac.in" has unnatural non-printable spaces behind it, so it doesn't directly work but works after backspacing and adding normal spaces back again.

8. since table is a multiline thing, I enforced that the starting << must be the first characters on the line to start a table format.  and no leading spaces should be added

6. underlining is delimited by space. An underscore after the underlined text is not necessary and a simple space can signify the end of  the underlining, but an underscore before the text is necessary. Hence  \_under\_line will also output as <u>under line</u> (a space is  there as well, since thats how its given in the Assignment.). Its important to note that either newline can only delimit it iff theres an  ending underscore as well, so ending with "\_\n" works fine but without the underscore it would instead go to the newline

2. Nested lists are a little weird as done by John Gruber. Its unclear how many spaces are needed for each nest. In markdown one space   would be enough to make a nested list (1st degree), but if you add 2 or 3 spaces before next element it wouldn't nest another list and  make a 2nd degree list but instead be considered as part of the same 1st degree list. It takes 5 spaces for a 2nd degree nested list to  form in markdown. example is given as the next list, of which you can see the source of. 
 But the way I have implemented is as follows
 - the number of spaces behind the start of the numbering, denotes the number of nests, so if there was " 1." written at the start then it would be considered to be a nest inside the first list point above it that has fewer leading spaces. and "  1." would be nested inside the first point above it with lesser than 2 leading spaces and so on. this makes it easy to nest. 
 - each paragraph has a blank line in between, like original markdown.
 - all the text in the same point should start after (n+1) spaces from the left, if we are in the nth nested list. so for an unnested list, all the lines should begin with 1 space to be considered part of the list point, otherwise the list would be ended and a paragraph started.

3. example Markdown List to show the unclear number of leading spaces it takes for nesting multiple levels
 - 1space
  - 2space
   - 3space
    - 4space
     - 5space

