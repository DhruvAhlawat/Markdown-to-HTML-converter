(* include IMPERATIVE_IO *)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph or codeblock, 
3 denotes if we are in bold, 4 denotes if we are in italics*)


fun mdt2html(infile) =
    let
        val ins = TextIO.openIn infile;
        val outs = TextIO.openOut "filename.html";

        (* fun bold(#"*" :: #"*" :: t) *)
        exception AsterixNotMatched;
        fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
        |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");
    
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


        fun leadingSpaces(#" " :: t, cnt) = leadingSpaces(t, cnt + 1)
        |   leadingSpaces(h::t,cnt) = (cnt,h::t);

        fun checkListItem(#"." :: #" " :: t, cnt) = (cnt, t)
        |   checkListItem(h::t,cnt) = if (Char.ord(h) <= 57 andalso Char.ord(h) >= 48) then checkListItem(t,cnt+1)
            else (0,t);
        
        



        fun header(#"#"::t,cnt,1,c,d,e) = (TextIO.output(outs,"</p>\n"); header(#"#"::t,cnt,0,c,d,e)) 
        |   header(#"#"::t,cnt,2,c,d,e) = (TextIO.output(outs,"</code></pre>\n"); header(#"#"::t,cnt,0,c,d,e))
        |   header(#"#"::t,cnt,0,c,d,e) = if (cnt < 5) then header(t,cnt+1,0,c,d,e)
        else (TextIO.output(outs,HeadString(6,0)); Parse(t,6,0,c,d,e); TextIO.output(outs,HeadString(6,1)); (0,c,d,e))
        |   header(s,0,a,c,d,e) = Parse(s,0,a,c,d,e)
        (*|   header(s,0,)*)
        |   header(s,cnt,a,c,d,e) = (TextIO.output(outs,HeadString(cnt,0)); Parse(s,cnt,0,c,d,e); TextIO.output(outs,HeadString(cnt,1)); (0,c,d,e))

        (* fun ListHandler(1,s,b,c,d,e,f) = 
        let
            TextIO.output(outs,"<ol><li>"); 
            val (bn,cn,dn,en) = Parse(s,0,b,c,d,e); 
            val toParseNext = LineWork(inputLine ins, bn, cn, dn, en,f);
            TextIO.output(outs,"/ol">)
        in 
            LineWork(toParseNext) (*reduce f*)
        |   ListHandler(0,s,b,c,d,e,f) =
        let
            TextIO.output(outs,"<li>"); 
            val (bn,cn,dn,en) = Parse(s,0,b,c,d,e); 
            val toParseNext = LineWork(inputLine ins, bn, cn, dn, en,f);
        in
            ListHandler(toParseNext)
        end; *)

        fun ListHandler(1,s,b,c,d,e,f) = (TextIO.output(outs,"<ol><li>"); Parse(s,0,b,c,d,e)) 
        |   ListHandler(0,s,b,c,d,e,f) = (TextIO.output(outs,"<li>"); Parse(s,0,b,c,d,e));
    

        fun LineWork(NONE,0,0,0,0,0) = (TextIO.closeIn ins; TextIO.closeOut outs) (*passes the entire state*)
        |   LineWork(NONE,1,0,0,0,f) = (TextIO.output(outs,"</p>\n"); LineWork(NONE,0,0,0,0,f)) (*closes the open paragraph*)
        |   LineWork(NONE,2,0,0,0,f) = (TextIO.output(outs,"</code></pre>"); LineWork(NONE,0,0,0,0,f))
        |   LineWork(NONE,b,c,d,0,f) = (TextIO.output(outs, "Asterix wasnt matched"); LineWork(NONE,0,0,0,0,f); raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d,e,f) = (TextIO.output(outs,"</u>"); LineWork(NONE,b,c,d,0,f)) (*inserts an underline ending automatically*)
        |   LineWork(SOME("\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),1,c,d,e,f) = (TextIO.output(outs,"</p>\n<hr>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e,f))
        |   LineWork(SOME("---\n"),2,c,d,e,f)= (TextIO.output(outs,"/code></pre>\n<hr>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f))
        |   LineWork(SOME("---\n"),b,c,d,e,f)= (TextIO.output(outs,"<hr>\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f))
        |   LineWork(SOME(line),b,c,d,e,0) = 
            let 
                val lspaces = leadingSpaces(explode line,0);
                val isListItem = checkListItem(#2 lspaces, 0);
            in 
                if(#1 isListItem = 0)  (*not a list, work normally*)
                then
                    let  
                        val (isInPara, isBold, isItalic, isUnderlined) = header(explode line,0,b,c,d,e);
                    in
                    LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined,0)
                    end
                else
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>") 
                        else ();
                        val (bl,cl,dl,el) = ListHandler(1,#2 isListItem,0,c,d,e,1); (*now we are in 1 level of list*)
                    in
                        LineWork(TextIO.inputLine ins,bl,cl,dl,el,1)
                    end
            end
        |   LineWork(SOME(line),b,c,d,e,f) = 
            let 
                val lspaces = leadingSpaces(explode line,0)
                val isListItem = checkListItem(#2 lspaces, 0)
            in
                if (#1 isListItem = 0)  (*not a list, so we close the list*)
                then
                    (TextIO.output(outs,"</li></ol>"); LineWork(SOME(line),b,c,d,e,f-1)) 
                else (*is a list*)
                    if(#1 lspaces >= f) 
                    then let
                        val (bl,cl,dl,el) = ListHandler(1,#2 isListItem,b,c,d,e,f+1);
                    in
                        LineWork(TextIO.inputLine ins, bl,cl,dl,el,f+1)
                    end
                    else
                    if(#1 lspaces = f-1) then 
                    let
                        val ok = TextIO.output(outs,"</li>");
                        val (bl,cl,dl,el) = ListHandler(0,#2 isListItem,b,c,d,e,f); 
                    in
                        LineWork(TextIO.inputLine ins, bl,cl,dl,el,f)
                    end
                    else 
                    let
                        val ok = TextIO.output(outs,"</li></ol></li>");
                        (* val (bl,cl,dl,el) = ListHandler(0,#2 isListItem,b,c,d,e,f-1); *)
                    in
                        LineWork(SOME(line),b,c,d,e,f-1)
                    end
            end
                


    in
        LineWork(TextIO.inputLine ins, 0,0,0,0,0)
    end;


(* 
val ins = TextIO.openIn "MarkdownTest.md";
val outs = TextIO.openOut "filename.html"; 
*)


mdt2html "ExampleFile.md";
mdt2html "MarkdownTest.md";
