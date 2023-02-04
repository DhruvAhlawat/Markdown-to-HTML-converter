
include IMPERATIVE_IO

exception AsterixNotMatched;
fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
    |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");

HeadString(6,1)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph or codeblock, 
3 denotes if we are in bold, 4 denotes if we are in italics*)


val ins = TextIO.openIn "MarkdownTest.md";
fun mdt2html(infile) =
    let
        val ins = TextIO.openIn infile;
        val outs = TextIO.openOut "filename.html";

        (* fun bold(#"*" :: #"*" :: t) *)

        fun Links(#"<"::s,toDisplay) = (TextIO.output(outs,"<a href=\""); Links(s,toDisplay)) (*required links to have http at the front*)
        |   Links(#">"::s,toDisplay) = (TextIO.output(outs, toDisplay^"\">"^toDisplay^"</a>"); s)
        |   Links(h::s,toDisplay) = Links(s,(toDisplay^String.str(h)));
        (*comments*)
        fun escape(#"." :: t ) = (TextIO.output1(outs,#"."); t) (*chr(92) is \ (backslash) and hence is used to escape characters*)
        |   escape(#"\\" ::  t ) = (TextIO.output1(outs,#"\\"); t) (*if \ is followed by a non-escapable character then it is printed normally*)
        |   escape(#"*" :: t ) = (TextIO.output1(outs,#"*"); t)
        |   escape( #"_" :: t ) = (TextIO.output1(outs,#"_"); t)
        |   escape( #"{" :: t ) = (TextIO.output1(outs,#"{"); t)
        |   escape( #"}" :: t ) = (TextIO.output1(outs,#"}"); t)
        |   escape( #"#" :: t ) = (TextIO.output1(outs,#"#"); t)
        |   escape( #"[" :: t ) = (TextIO.output1(outs,#"["); t)
        |   escape( #"(" :: t ) = (TextIO.output1(outs,#"("); t)
        |   escape( #")" :: t ) = (TextIO.output1(outs,#")"); t)
        |   escape( #"]" :: t ) = (TextIO.output1(outs,#"]"); t)
        |   escape( #"-" :: t ) = (TextIO.output1(outs,#"-"); t)
        |   escape( #"+" :: t ) = (TextIO.output1(outs,#"+"); t)
        |   escape( #"!" :: t ) = (TextIO.output1(outs,#"!"); t)
        |   escape( #"`" :: t ) = (TextIO.output1(outs,#"`"); t)
        |   escape(t) = t; (*printed normally if inescapable character*)

       

        fun codeBlock([]) = 1
        |   codeBlock(#"<" :: t) = (TextIO.output(outs,"&lt"); codeBlock(t)) (*inside codeblocks, <,> and & are automatically converted *)
        |   codeBlock(#">" :: t) = (TextIO.output(outs,"&gt"); codeBlock(t))
        |   codeBlock(#"&" :: t) = (TextIO.output(outs,"&amp"); codeBlock(t))
        |   codeBlock(s::t) = (TextIO.output1(outs,s); codeBlock(t));

        fun Parse([],a,b,c,d,e) = (b,c,d,e) (*empty so we should just return if we are in a paragraph*)
        |   Parse(#" " :: #" " :: #" " :: #" " :: t,0,0,c,d,e) = (TextIO.output(outs,"<pre><code>"); codeBlock(t); (2,c,d,e))
        |   Parse(#" " :: #" " :: #" " :: #" " :: t,0,1,c,d,e) = (TextIO.output(outs,"</p>\n<pre><code>"); codeBlock(t); (2,c,d,e)) (*Transitions to state 2 which means we are in codeblocks*)
        |   Parse(#" " :: #" " :: #" " :: #" " :: t,0,2,c,d,e) = (codeBlock(t); (2,c,d,e))
        |   Parse(h::t,0,2,c,d,e) = (TextIO.output(outs,"</code></pre>\n"); Parse(h::t,0,0,c,d,e))
        |   Parse(h::t,0,0,c,d,e) = (TextIO.output(outs,"<p>\n"); Parse(h::t,0,1,c,d,e))
        |   Parse(#"\\" :: t,a,b,c,d,e) = Parse(escape(t),a,b,c,d,e)
        |   Parse(#"<" :: #"h" :: #"t":: #"t" :: #"p":: t,a,b,c,d,e) = Parse(Links(#"<" :: #"h" :: #"t" :: #"t" :: #"p":: t,""),a,b,c,d,e)
        |   Parse(#"*" :: #"*" :: t,a,b,0,d,e) = (TextIO.output(outs,"<strong>"); Parse(t,a,b,1,d,e))
        |   Parse(#"*" :: #"*" :: t,a,b,1,d,e) = (TextIO.output(outs,"</strong>"); Parse(t,a,b,0,d,e))
        |   Parse(#"*" :: t,a,b,c,0,e) = (TextIO.output(outs,"<em>"); Parse(t,a,b,c,1,e))
        |   Parse(#"*" :: t,a,b,c,1,e) = (TextIO.output(outs,"</em>"); Parse(t,a,b,c,0,e))
        |   Parse(#"_" :: t,a,b,c,d,0) = (TextIO.output(outs,"<u>"); Parse(t,a,b,c,d,1))
        |   Parse(#" " :: t,a,b,c,d,1) = (TextIO.output(outs,"</u>"); Parse(t,a,b,c,d,0))
        |   Parse(h::t,a,b,c,d,e) = (TextIO.output1(outs,h); Parse(t,a+1,1,c,d,e));
        
        fun header(#"#"::t,cnt,1,c,d,e) = (TextIO.output(outs,"</p>\n"); header(#"#"::t,cnt,0,c,d,e)) 
        |   header(#"#"::t,cnt,2,c,d,e) = (TextIO.output(outs,"</code></pre>\n"); header(#"#"::t,cnt,0,c,d,e))
        |   header(#"#"::t,cnt,0,c,d,e) = if (cnt < 5) then header(t,cnt+1,0,c,d,e)
        else (TextIO.output(outs,HeadString(6,0)); Parse(t,6,0,c,d,e); TextIO.output(outs,HeadString(6,1)); (0,c,d,e))
        |   header(s,0,a,c,d,e) = Parse(s,0,a,c,d,e)
        (*|   header(s,0,)*)
        |   header(s,cnt,a,c,d,e) = (TextIO.output(outs,HeadString(cnt,0)); Parse(s,cnt,0,c,d,e); TextIO.output(outs,HeadString(cnt,1)); (0,c,d,e))

        fun leadingSpaces(#" " :: t, cnt) = leadingSpaces(t, cnt + 1)
        |   leadingSpaces(h::t,cnt) = (cnt,t);

        fun checkListItem(#"." :: #" " :: t, cnt) = (cnt, t)
        |   checkListItem(h::t,cnt) = if (Char.ord(h) <= 57 andalso Char.ord(h) >= 48) then checkListItem(t,cnt+1)
        else (0,t);
        
        fun ListHandling(line,f) = (TextIO.output(outs, "<li>"); )


        fun closeOLists(0) = 0
        |   closeOLists(k) = (TextIO.output(outs,"</ol>"); k-1);

        fun LineWork(NONE,0,0,0,0,0) = (TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(NONE,1,0,0,0,f) = (TextIO.output(outs,"</p>\n"); LineWork(NONE,0,0,0,0,f)) (*closes the open paragraph*)
        |   LineWork(NONE,2,0,0,0,f) = (TextIO.output(outs,"</code></pre>"); LineWork(NONE,0,0,0,0,f))
        |   LineWork(NONE,b,c,d,0,f) = (TextIO.output(outs, "Asterix wasnt matched"); LineWork(NONE,0,0,0,0,f); raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d,e,f) = (TextIO.output(outs,"</u>"); LineWork(NONE,b,c,d,0,f)) (*inserts an underline ending automatically*)
        |   LineWork(SOME("\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n<hr>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e,f))
        |   LineWork(SOME("---\n"),2,c,d,e,f)= (TextIO.output(outs,"/code></pre>\n<hr>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),b,c,d,e,f)= (TextIO.output(outs,"<hr>\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f))

        |   LineWork(SOME(line),b,c,d,e,f) = (*this should be for f = 0 btw, if f is not 0 then we need additional checks for whether to go intos*)
            let 
                val lspaces = leadingSpaces(explode line,0);
                val isListItem = checkListItem(#2 lspaces, 0);
            in
                if(#1 isListItem = 0) then (*this should be a normal paragraph now, hence we should first end all the lists previously opened*)
                let
                    val (isInPara, isBold, isItalic, isUnderlined) = header(explode line,0,b,c,d,e)
                    closeOLists(f); 
                in
                    LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined, 0)
                end
                else (*else it is a list item so we gotta do what we gotta do*)
                let
                    val kk = if(#1 lspaces > f) then (TextIO.output(outs,"<ol><li>"); )
                    else if(#1 lspaces = f) then (TextIO.output(outs,"<li>")
                in
                    (*then we gotta make another list as this is an indented list item*)

                    else (*then we gotta make another list item instead*)
                    LineWork(TextIO.inputLine ins, 0, 0, 0, 0, f)
                end
            end
            
    in
        LineWork(TextIO.inputLine ins, 0,0,0,0,0)
    end;



val ins = TextIO.openIn "MarkdownTest.md";
val outs = TextIO.openOut "filename.html";


mdt2html "MarkdownTest.md";
mdt2html "ExampleFile.md";

