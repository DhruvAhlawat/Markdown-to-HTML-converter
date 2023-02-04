 fun LineWork(NONE,0,0,0,0,0) = (TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(NONE,1,0,0,0,f) = (TextIO.output(outs,"</p>\n"); LineWork(NONE,0,0,0,0,f)) (*closes the open paragraph*)
        |   LineWork(NONE,2,0,0,0,f) = (TextIO.output(outs,"</code></pre>"); LineWork(NONE,0,0,0,0,f))
        |   LineWork(NONE,b,c,d,0,f) = (TextIO.output(outs, "Asterix wasnt matched"); LineWork(NONE,0,0,0,0,f); raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d,e,f) = (TextIO.output(outs,"</u>"); LineWork(NONE,b,c,d,0,f)) (*inserts an underline ending automatically*)
        |   LineWork(SOME("\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n<hr>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e,f))
        |   LineWork(SOME("---\n"),2,c,d,e,f)= (TextIO.output(outs,"/code></pre>\n<hr>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),b,c,d,e,f)= (TextIO.output(outs,"<hr>\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f))

        |   LineWork(SOME(line),b,c,d,e,f) = if(#1 (checkListItem(#2 (leadingSpaces(explode line,0)),0)) = 0) 
            then (*then we are not inside a list rn*)
            let
                val (isInPara, isBold, isItalic, isUnderlined) = header(explode line,0,b,c,d,e)
                val isInsideList = f;
                in
                LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined, isInsideList)
            end 
            else 
                LineWork(TextIO.inputLine ins, 0, 0, 0, 0, 0);
            
    in
        LineWork(TextIO.inputLine ins, 0,0,0,0,0)
    end;