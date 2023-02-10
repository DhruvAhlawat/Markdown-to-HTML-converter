(* include IMPERATIVE_IO *)

(*throughout the code, the letter b stands for whether we are in a paragraph or not, c stands for bold, d for italics, e for underline*)
(**)
fun mdt2html(infile) =
    let
        val ins = TextIO.openIn (infile^".mdt");
        val outs = TextIO.openOut (infile^".html");

        (* fun bold(#"*" :: #"*" :: t) *)
        exception AsterixNotMatched;
        exception BracketErrorInLinkPlacement;
        exception underlineOrBoldOrItalicNotEndedInsideTableCell;

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

        (*my naming conventions for b,c,d,e are given at the top, signifies what they mean*)
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
        |   Parse(#"_" :: (#" " | #"\n") :: t,a,b,c,d,1) = (TextIO.output(outs,"</u> "); Parse(#" "::t,a,b,c,d,0))
        |   Parse(#"_" :: t,a,b,c,d,1) = (TextIO.output1(outs,#" "); Parse(t,a,b,c,d,1))
        |   Parse(h::t,a,b,c,d,e) = (TextIO.output1(outs,h); Parse(t,a+1,1,c,d,e));

      


       


        fun leadingSpaces(#" " :: t, cnt) = leadingSpaces(t, cnt + 1)
        |   leadingSpaces(h::t,cnt) = (cnt,h::t);

        fun removeFirstK(h,0) = h
        |   removeFirstK(h::t,a) = removeFirstK(t,a-1);

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

       
        fun ListHandler(1,s,b,c,d,e) = (TextIO.output(outs,"<ol>"); ListHandler(0,s,b,c,d,e)) 
        |   ListHandler(0,s,b,c,d,e) = (TextIO.output(outs,"<li>"); Parse(s,0,b,c,d,e))
        |   ListHandler(2,s,b,c,d,e) = (TextIO.output(outs,"<ul>"); ListHandler(0,s,b,c,d,e)) (*starts an unordered list*)
    


        fun ParseTable([],a,b,c,d,e) = (b,c,d,e) (*empty so we should just return if we are in a paragraph*)
        |   ParseTable(h::t,0,b,c,d,e) = (TextIO.output(outs,"<TR><TD>"); ParseTable(h::t,1,b,c,d,e))
        |   ParseTable(#"|"::t,a,b,0,0,0) = (TextIO.output(outs,"</TD><TD>"); ParseTable(t,a+1,b,0,0,0))
        |   ParseTable(#"|"::t,a,b,c,d,e) = (TextIO.output(outs,"<strong>Please close all bolds, underlines (with a space at the end) and italics within the cell of the table it was opened in</strong>"); raise underlineOrBoldOrItalicNotEndedInsideTableCell) (*the * or _ isnt properly closed*)
        |   ParseTable(#"\\" :: t,a,b,c,d,e) = ParseTable(escape(t),a,b,c,d,e) (*escape characters*)
        |   ParseTable(#"*" :: #"*" :: t,a,b,0,d,e) = (TextIO.output(outs,"<strong>"); ParseTable(t,a+1,b,1,d,e))
        |   ParseTable(#"*" :: #"*" :: t,a,b,1,d,e) = (TextIO.output(outs,"</strong>"); ParseTable(t,a+1,b,0,d,e))
        |   ParseTable(#"*" :: t,a,b,c,0,e) = (TextIO.output(outs,"<em>"); ParseTable(t,a+1,b,c,1,e))
        |   ParseTable(#"*" :: t,a,b,c,1,e) = (TextIO.output(outs,"</em>"); ParseTable(t,a+1,b,c,0,e))
        |   ParseTable(#"_" :: t,a,b,c,d,0) = (TextIO.output(outs,"<u>"); ParseTable(t,a+1,b,c,d,1))
        |   ParseTable(#"_" :: (#" " | #"\n") :: t,a,b,c,d,1) = (TextIO.output(outs,"</u> "); ParseTable(#" "::t,a+1,b,c,d,0))
        |   ParseTable(#"_" :: t,a,b,c,d,1) = (TextIO.output1(outs,#" "); ParseTable(t,a,b,c,d,1))
        |   ParseTable([#"\n"],a,b,0,0,0) = (TextIO.output(outs,"</TD></TR>\n"); (b,0,0,0))
        |   ParseTable([#"\n"],a,b,c,d,e) = (TextIO.output(outs,"<strong>Please close all bolds, underlines (with a space at the end) and italics within the cell of the table it was opened in</strong>"); raise underlineOrBoldOrItalicNotEndedInsideTableCell) (*the * or _ isnt properly closed*)
        |   ParseTable(h::t,a,b,c,d,e) = (TextIO.output1(outs,h); ParseTable(t,a+1,b,c,d,e));

        fun getExplodedNextLine() = 
        let 
            val SOME(nl) = TextIO.inputLine ins;
        in
            explode nl
        end;

        fun Table(#"<":: #"<" :: t) = (TextIO.output(outs,"<CENTER><TABLE border= \"1\">"); Table(getExplodedNextLine()))
        |   Table(#">":: #">" :: t) = TextIO.output(outs,"</TABLE></CENTER>")
        |   Table(s) = (ParseTable(s,0,0,0,0,0); Table(getExplodedNextLine()));
        fun countgt(cnt,#">"::t) = countgt(cnt+1,t)
        |   countgt(cnt,genericList) = (cnt,genericList);

        fun repeatBQout(0,a) = ()
        |   repeatBQout(a,0) = (TextIO.output(outs,"\n<blockquote>"); repeatBQout(a-1,0))
        |   repeatBQout(a,b) = (TextIO.output(outs,"</blockquote>\n"); repeatBQout(a-1,1));
        
        fun LineWork(NONE,0,0,0,0,0,g,0,i) = (TextIO.closeIn ins; TextIO.closeOut outs) (*passes the entire state*)
        |   LineWork(NONE,0,0,0,0,f,g,0,i) = ((if (hd(g) = 1) then TextIO.output(outs,"</li></ol>") else TextIO.output(outs,"</li></ul>")); LineWork(NONE,0,0,0,0,f-1,tl(g),0,i)) 
        (* |   LineWork(NONE,0,0,0,0,f) =  *) (*gotta implement recursive closure of lists at the end*)
        |   LineWork(NONE,0,0,0,0,f,g,h,i) = (repeatBQout(h,1);LineWork(NONE,0,0,0,0,f,g,0,i))
        |   LineWork(NONE,1,0,0,0,f,g,h,i) = (TextIO.output(outs,"</p>\n"); LineWork(NONE,0,0,0,0,f,g,h,i)) (*closes the open paragraph*)
        |   LineWork(NONE,2,0,0,0,f,g,h,i) = (TextIO.output(outs,"</code></pre>"); LineWork(NONE,0,0,0,0,f,g,h,i))
        |   LineWork(NONE,b,c,d,0,f,g,h,i) = (TextIO.output(outs, "Asterix wasnt matched"); LineWork(NONE,0,0,0,0,f,g,h,i); raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d,e,f,g,h,i) = (TextIO.output(outs,"</u>"); LineWork(NONE,b,c,d,0,f,g,h,i)) (*inserts an underline ending automatically*)
        (* |   LineWork(SOME(line),b,c,d,e,f,g,0,1) =  LineWork(SOME(line),b,c,d,e,f,g,0,0) *)
        |   LineWork(SOME(line),b,c,d,e,f,g,h,1) = 
            let
                val expLine = explode(line);
                val (cnt,onRight) = countgt(0,expLine);
                val lineOption = SOME(implode(onRight));
            in
                if(cnt > h) then (repeatBQout(cnt-h,0); LineWork(lineOption,b,c,d,e,f,g,cnt,0)) 
                else 
                (if (cnt = h) then LineWork(lineOption,b,c,d,e,f,g,cnt,0) 
                else (repeatBQout(h-cnt,1); LineWork(lineOption,b,c,d,e,f,g,cnt,0))) 
                    (* LineWork(SOME(line),b,c,d,e,f,g,h,0) *)
            end
        |   LineWork(SOME("<<\n"),b,c,d,e,f,g,h,i) = (Table(explode "<<\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f,g,h,i))
        |   LineWork(SOME("\n"),1,c,d,e,f,g,h,i) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f,g,h,i))
        |   LineWork(SOME("---\n"),1,c,d,e,f,g,h,i) = (TextIO.output(outs,"</p>\n<hr>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e,f,g,h,i))
        |   LineWork(SOME("---\n"),2,c,d,e,f,g,h,i)= (TextIO.output(outs,"/code></pre>\n<hr>\n"); LineWork(TextIO.inputLine ins,0,c,d,e,f,g,h,i))
        |   LineWork(SOME("---\n"),b,c,d,e,f,g,h,i)= (TextIO.output(outs,"<hr>\n"); LineWork(TextIO.inputLine ins,b,c,d,e,f,g,h,i))
        |   LineWork(SOME(line),b,c,d,e,0,g,h,0) = 
            let 
                val explodedLine = explode line;
                val lspaces = leadingSpaces(explodedLine,0);
                val isListItem = checkListItem(#2 lspaces);
            in 
                if(#1 isListItem = 0)  (*not a list, work normally*)
                then
                    let  
                        val b = if(b = 1 andalso #2 lspaces = [#"\n"]) then (TextIO.output(outs,"</p>");0) else b;
                        val (isInPara, isBold, isItalic, isUnderlined) = header(explodedLine,0,b,c,d,e);
                    in
                        LineWork(TextIO.inputLine ins,isInPara , isBold, isItalic, isUnderlined,0,g,h,1)
                    end
                else
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                        val (bl,cl,dl,el) = ListHandler(#3 isListItem,#2 isListItem,0,c,d,e); (*#3 isListItem denotes the type of list,ol or ul*)
                             (*now we are in 1 level of list*)
                    in
                        LineWork(TextIO.inputLine ins,bl,cl,dl,el,1,(#3 isListItem) :: g,h,1)
                    end
            end 
        |   LineWork(SOME(line),b,c,d,e,f,g,h,0) = 
            let 
                val explodedLine = explode line;
                val lspaces = leadingSpaces(explodedLine,0)
                val isListItem = checkListItem(#2 lspaces)
            in
                if(#2 lspaces = [#"\n"]) then (*then it is just a blank line, so we should not close the list *)
                    (*we should just continue ahead in this case as its a blank line only, no need to end our list*)
                    (if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                    LineWork(TextIO.inputLine ins,0,c,d,e,f,g,h,1))
                else if (#1 isListItem = 0 andalso #1 lspaces  < f)  (*not a current degree(f) list, so we close the list*)
                then
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                    in
                        if(hd(g) = 1) then (TextIO.output(outs,"</li></ol>"); LineWork(SOME(line),0,c,d,e,f-1,tl(g),h,0)) 
                        else (TextIO.output(outs,"</li></ul>"); LineWork(SOME(line),0,c,d,e,f-1,tl(g),h,0))
                    end
                else if(#1 isListItem = 0 andalso #1 lspaces >= f) then
                    (*then we should parse this normally without applying the tags*)
                    let  
                        val (isInPara, isBold, isItalic, isUnderlined) = header(removeFirstK(explodedLine,f),0,b,c,d,e);
                    in
                        LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined,f,g,h,1)
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
                       LineWork(TextIO.inputLine ins, b1,cl,dl,el,f+1,(#3 isListItem)::g,h,1)
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
                        LineWork(TextIO.inputLine ins, bl,cl,dl,el,f,g,h,1)
                    end
                    else 
                    let 
                        val tempor = if (b = 1) then TextIO.output(outs,"</p>\n") else 
                        if (b = 2) then TextIO.output(outs,"</code></pre>")  (*closes previously open stuff*)
                        else ();
                        val ok = if(hd(g) = 1) then TextIO.output(outs,"</li></ol>") else TextIO.output(outs,"</li></ul>");
                        (* val (bl,cl,dl,el) = ListHandler(0,#2 isListItem,b,c,d,e); *)
                    in
                        LineWork(SOME(line),b,c,d,e,f-1,tl(g),h,0)
                    end
            end
    in
        LineWork(TextIO.inputLine ins, 0,0,0,0,0,[],0,1)
    end;

(*tbh, I probably could have implemented this much better. Instead of a self recursively calling function like Linework, I should have instead returned its
parameter values and let an outer function handle the rest and call it again. That way I could have encapsulated LineWork into any other
framework as well, calling LineWork when necessary. But as it stands, LineWork once called will keep on calling itself till the file ends
But I have already come so far so I decided to go with how it is. Still, I am glad I was able to finish it just 30mins before the deadline,
and I had quite a lotta fun as it required a lot of thinking*)


(* mdt2html "README"; *)

