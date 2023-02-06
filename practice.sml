(* include IMPERATIVE_IO *)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph or codeblock, 
3 denotes if we are in bold, 4 denotes if we are in italics*)


fun mdt2html(infile) =
    let
        val ins = TextIO.openIn infile;
        val outs = TextIO.openOut "filename.html";

        (* fun bold(#"*" :: #"*" :: t) *)
        exception AsterixNotMatched;
        exception BracketErrorInLinkPlacement;

        fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
        |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");
    
        fun Links(#"<"::s,toDisplay) = (TextIO.output(outs,"<a href=\""); Links(s,toDisplay)) (*required links to have http at the front*)
        |   Links(#">"::s,toDisplay) = (TextIO.output(outs, toDisplay^"\">"^toDisplay^"</a>"); s)
        |   Links(h::s,toDisplay) = Links(s,(toDisplay^String.str(h)));
        (*comments*)

        fun LinkBrackets([],tD,link,a) = raise BracketErrorInLinkPlacement (*error if brackets dont match*)
        (* |   LinkBrackets(#"(" :: s, tD, link, 0) = raise BracketErrorInLinkPlacement if ( or ) comes before [] closes *)
        |   LinkBrackets(#"["::s,toDisplay,link,a) = LinkBrackets(s,"","",0)
        |   LinkBrackets(#"]":: #"(" :: s,toDisplay,link,a) = LinkBrackets(s,toDisplay,link,1)
        |   LinkBrackets(#"]"::s,toDisplay,link,a) = (TextIO.output(outs,"["^toDisplay^"]"); s) (*incase ( doesnt come directly after ], then its assumed to not be a link, and the original text is pasted *)
        |   LinkBrackets(#")"::s,toDisplay,link,1) = (TextIO.output(outs,"<a href=\""^link^"\">"^toDisplay^"</a>"); s)
        |   LinkBrackets(h::t,toDisplay,link,0) = LinkBrackets(t,toDisplay^str(h),link,0)
        |   LinkBrackets(h::t,tD,link,a) = LinkBrackets(t,tD,link^String.str(h),1)

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
        |   Parse(#"\\" :: t,a,b,c,d,e) = Parse(escape(t),a,b,c,d,e) (*escape characters*)
        |   Parse(#"[" :: t,a,b,c,d,e) = Parse(LinkBrackets(#"["::t,"","",0), a,b,c,d,e)
        |   Parse(#"<" :: #"h" :: #"t":: #"t" :: #"p":: t,a,b,c,d,e) = Parse(Links(#"<" :: #"h" :: #"t" :: #"t" :: #"p":: t,""),a,b,c,d,e)
        |   Parse(#"*" :: #"*" :: t,a,b,0,d,e) = (TextIO.output(outs,"<strong>"); Parse(t,a,b,1,d,e))
        |   Parse(#"*" :: #"*" :: t,a,b,1,d,e) = (TextIO.output(outs,"</strong>"); Parse(t,a,b,0,d,e))
        |   Parse(#"*" :: t,a,b,c,0,e) = (TextIO.output(outs,"<em>"); Parse(t,a,b,c,1,e))
        |   Parse(#"*" :: t,a,b,c,1,e) = (TextIO.output(outs,"</em>"); Parse(t,a,b,c,0,e))
        |   Parse(#"_" :: t,a,b,c,d,0) = (TextIO.output(outs,"<u>"); Parse(t,a,b,c,d,1))
        |   Parse(#"_" :: (#" " | #"\n") :: t,a,b,c,d,1) = (TextIO.output(outs,"</u> "); Parse(t,a,b,c,d,0))
        |   Parse(h::t,a,b,c,d,e) = (TextIO.output1(outs,h); Parse(t,a+1,1,c,d,e));


        fun leadingSpaces(#" " :: t, cnt) = leadingSpaces(t, cnt + 1)
        |   leadingSpaces(h::t,cnt) = (cnt,h::t);

        fun checkOLItem(#"." :: #" " :: t, cnt) = (cnt, t)
        |   checkOLItem(h::t,cnt) = if (Char.ord(h) <= 57 andalso Char.ord(h) >= 48) then checkOLItem(t,cnt+1)
            else (0,t);

        fun checkULItem(#"-" :: #" " :: t) = (1, t)
        |   checkULItem(s) = (0,s);
        
        fun checkListItem(s) = 
        let 
            val UL = checkULItem(s);
        in
            if(#1 UL = 0) then 
                let 
                    val OL = checkOLItem(s,0);
                in
                    if(#1 OL = 0) then (0,#2 OL,1) else (*last parameter 1 means ordered list and 2 means unordered list*)
                    (1,#2 OL,1)
                end
            else (1,#2 UL,2)
        end;


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

        fun ListHandler(1,s,b,c,d,e) = (TextIO.output(outs,"<ol><li>"); Parse(s,0,b,c,d,e)) 
        |   ListHandler(0,s,b,c,d,e) = (TextIO.output(outs,"<li>"); Parse(s,0,b,c,d,e))
        |   ListHandler(2,s,b,c,d,e) = (TextIO.output(outs,"<ul><li>"); Parse(s,0,b,c,d,e)) (*starts an unordered list*)
    
       
        (*the last parameter of LineWork is a list whose head stores whether we are in ordered list or unordered list*)

        fun LineWork(NONE,0,0,0,0,f,g) = (TextIO.closeIn ins; TextIO.closeOut outs) (*passes the entire state*)
        (* |   LineWork(NONE,0,0,0,0,f) =  *) (*gotta implement recursive closure of lists at the end*)
        |   LineWork(NONE,1,0,0,0,f,g) = (TextIO.output(outs,"</p>\n"); LineWork(NONE,0,0,0,0,f,g)) (*closes the open paragraph*)
        |   LineWork(NONE,2,0,0,0,f,g) = (TextIO.output(outs,"</code></pre>"); LineWork(NONE,0,0,0,0,f,g))
        |   LineWork(NONE,b,c,d,0,f,g) = (TextIO.output(outs, "Asterix wasnt matched"); LineWork(NONE,0,0,0,0,f,g); raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d,e,f,g) = (TextIO.output(outs,"</u>"); LineWork(NONE,b,c,d,0,f,g)) (*inserts an underline ending automatically*)
        |   LineWork(SOME("\n"),1,c,d,e,f,g) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f,g))
        |   LineWork(SOME("---\n"),1,c,d,e,f,g) = (TextIO.output(outs,"</p>\n<hr>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e,f,g))
        |   LineWork(SOME("---\n"),2,c,d,e,f,g)= (TextIO.output(outs,"/code></pre>\n<hr>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f,g))
        |   LineWork(SOME("---\n"),b,c,d,e,f,g)= (TextIO.output(outs,"<hr>\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f,g))
        |   LineWork(SOME(line),b,c,d,e,0,g) = 
            let 
                val lspaces = leadingSpaces(explode line,0);
                val isListItem = checkListItem(#2 lspaces);
            in 
                if(#1 isListItem = 0)  (*not a list, work normally*)
                then
                    let  
                        val (isInPara, isBold, isItalic, isUnderlined) = header(explode line,0,b,c,d,e);
                    in
                    LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined,0,g)
                    end
                else
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                        val (bl,cl,dl,el) = ListHandler(#3 isListItem,#2 isListItem,0,c,d,e); (*#3 isListItem denotes the type of list,ol or ul*)
                             (*now we are in 1 level of list*)
                    in
                        LineWork(TextIO.inputLine ins,bl,cl,dl,el,1,(#3 isListItem) :: g)
                    end
            end 
        |   LineWork(SOME(line),b,c,d,e,f,g) = 
            let 
                val lspaces = leadingSpaces(explode line,0)
                val isListItem = checkListItem(#2 lspaces)
            in
                if(#2 lspaces = [#"\n"]) then (*then it is just a blank line, so we should not close the list *)
                    (*we should just continue ahead in this case as its a blank line only, no need to end our list*)
                    LineWork(TextIO.inputLine ins,b,c,d,e,f,g)
                else if (#1 isListItem = 0 andalso #1 lspaces  < f)  (*not a current degree(f) list, so we close the list*)
                then
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                    in
                        if(hd(g) = 1) then (TextIO.output(outs,"</li></ol>"); LineWork(SOME(line),0,c,d,e,f-1,tl(g))) 
                        else (TextIO.output(outs,"</li></ul>"); LineWork(SOME(line),0,c,d,e,f-1,tl(g)))
                    end
                else if(#1 isListItem = 0 andalso #1 lspaces >= f) then
                    (*then we should parse this normally without applying the tags*)
                    let  
                        val (isInPara, isBold, isItalic, isUnderlined) = header(#2 lspaces,0,b,c,d,e);
                    in
                    LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined,f,g)
                    end (*normal parsing*)
                else
                (*is a list*)
                    if(#1 lspaces >= f) 
                    then let
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else (); (*ending the previous paragraph*)
                        val (b1,cl,dl,el) = ListHandler(#3 isListItem,#2 isListItem,0,c,d,e);
                    in
                       LineWork(TextIO.inputLine ins, b1,cl,dl,el,f+1,(#3 isListItem)::g)
                    end
                    else
                    if(#1 lspaces = f-1) then 
                    let
                        (* val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else (); *)
                        val ok = TextIO.output(outs,"</li>");
                        val (bl,cl,dl,el) = ListHandler(0,#2 isListItem,b,c,d,e); 
                    in
                        LineWork(TextIO.inputLine ins, bl,cl,dl,el,f,g)
                    end
                    else 
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                        val ok = if(hd(g) = 1) then TextIO.output(outs,"</li></ol></li>") else TextIO.output(outs,"</li></ul></li>");
                        (* val (bl,cl,dl,el) = ListHandler(0,#2 isListItem,b,c,d,e); *)
                    in
                        LineWork(SOME(line),b,c,d,e,f-1,tl(g))
                    end
            end
                
    in
        LineWork(TextIO.inputLine ins, 0,0,0,0,0,[])
    end;


(* 
val ins = TextIO.openIn "MarkdownTest.md";
val outs = TextIO.openOut "filename.html"; 
*)


mdt2html "ExampleFile.md"; 
mdt2html "MarkdownTest.md";

