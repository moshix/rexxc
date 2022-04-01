/* REXXC                               */
/*                                     */
/* A REXX compiler with x86 and z      */
/* backends                            */
/* PeterJ  &  Moshix                   */
/*                                     */
/* copyright 2022                      */
/* GPL 3 license                       */
/***************************************/
/*  CHANGE HISTORY                                                   */
/*  V0.1    :  Humble beginnings                                     */
/*  V0.2    :  time keeping, help for user                           */
/*  V0.3    :  lexicon in raw format, needs to convert into rexx stem*/
/*  V0.4    :  Peter's first parser                                  */
/*  V0.5    :                                                        */
 
 
/* configuraiton parameters - IMPORTANT                              */
version="0.4"              /*                                        */
log2file=0                 /* log everything to $file.log            */
 
/* global variables                                                  */
loggedonusers = 0        /* online user at any given moment          */
totmessages  = 0         /* total number of msgs sent                */
totwmessages = 0         /* total warning messages in this run       */
totemessages = 0         /* total error messages in this run         */
totimessages = 0         /* total of informational message           */
starttime=mytime()       /* when this compiler started               */
starttimeSEC=ExTime()    /* .... in seconds                          */
logline = " "            /* initialize log line                      */
 
 
 
call initrexxc           /* initalize rexxc  and do all the fancy stuff to start */
call readlex             /* read in lexicon into memory                          */
call readsource          /* read source into memory                              */
call parse               /* parse code                                           */ 
 
 
xit:
/* when its time to quit, come here    */
say "Exiting REXXC now..." 
exit;
 
parse: 
/* parses REXX source code */
 
do until string=''                                                
   si=si+1                                                        
   stmt=instruction(string)                                       
   if strip(stmt)=''          then iterate                        
   else if dostmt(stmt)  =1   then say "DO discovered :"stmt      
   else if setstmt(stmt) =1   then say "SET discovered :"stmt     
   else if ifstmt(stmt)  =1   then say "IF  discovered :"stmt     
   else if callstmt(stmt)=1   then say "CALL discovered :"stmt    
   else if endstmt(stmt) =1   then say "END discovered :"stmt     
   else if saystmt(stmt) =1   then say "SAY discovered :"stmt     
   else if returnstmt(stmt)=1 then say "RETURN discovered :"stmt  
   else if exitstmt(stmt)=1   then say "EXIT discovered :"stmt    
   else say '++++ unknown "'stmt"'"                               
 end    
return
 
helpuser:
/* send help menu */
say 'REXXC - Help '
say 'Invoke with rexxc sourcefile outputbinary'
say '...'
say 'outputbinary.listing will include compiler listing and assembler'
say ' '
 
return
 
exTime:
/* Calculate Seconds in this year */
  dd=(date('d')-1)*86400
  parse value time() with hh':'mm':'ss
  tt=hh*3600+mm*60+ss
return right(dd+tt,8,'0') 
 
epochat: procedure
/*return future epoch time based on date       */
parse arg futuretime
 
today = Date('Base')
days = today - Date('Base', '20210101', 'Standard')
/* TIME IS 00:09:05 EDT WEDNESDAY 03/03/21 */
qtime=futuretime
parse var qtime hh':'mm':'ss
tt=hh*3600+mm*60+ss
dd=days*86400
sincepoch=tt+dd
return sincepoch
 
mytime: procedure
 timenow = left(time(),5)
 hr = left(timenow,2)
 min = right(timenow,2)
 if hr > 12 then timenow         = (hr - 12)'.'min' pm'
   else if hr = 12 then timenow  = hr'.'min' pm'
                    else timenow = hr'.'min' am'
 if left(timenow,1) = '0' then timenow = substr(timenow,2)
 dow     = left(date('weekday'),3)
 day     = right(date('sorted'),2)
 if left(day,1) = '0' then day = substr(day,2)
 month   = left(date('month'),3)
 year    = left(date('sorted'),4)
return timenow',' dow day month year
 
log:
/* general logger function                */
/* log a line to console and listing file */
   parse ARG  logline
   say mytime()' :: 'logline
   if log2file = 1 & compatibility > 0 then do
   address command
/*  'PIPE (name logit)',
     '| spec /'mytime()'/ 1 /::/ n /'logLine'/ n',
     '| >> BUTLER LOG A'*/
/*
   logline=mytime()||' :: '||logline
     'EXECIO 1 DISKW RELAY LOG A (STRING '||logline
     'FINIS BUTLER LOG A' */
   end  
return
 
 
 
initrexxc:
/* rexxc  initialization routimes */
 
 
/* some simple logging  for stats etc        */
      CALL log('REXXC  '||version||' started. ')
 
 
 
/* init double linked list of online users   */
call @init
 CALL log('List has been initialized.')
 CALL log('List size: '||@size())
 if @size() != 0 then do
   CALL log('Linked list init has failed! Abort ')
   signal xit;
end
 
 
 
CALL log('********** REXXC   START **********')
 say '                                                            '
 say '  REXXC v 'version ' starting...                            '
 
return
 
p:      return word(arg(1), 1)    /*pick the first word out of many items*/
sy:      say;
         say left('', 30) "   " arg(1) '   ';
         return
@init:   $.@=;
        @adjust: $.@=space($.@);
        $.#=words($.@);
        return
@hasopt: arg o;
        return pos(o, opt)\==0
@size:  return $.#
 
/*                                        */
@del:   procedure expose $.;
        arg k,m;
        call @parms 'km'
         _=subword($.@, k, k-1)   subword($.@, k+m)
         $.@=_;
         call @adjust;
        return
 
@get:    procedure expose $.;     arg k,m,dir,_
         call @parms 'kmd'
         do j=k  for m  by dir  while  j>0  &  j<=$.#
             _=_ subword($.@, j, 1)
        end   /*j*/
         return strip(_)
 
@parms:  arg opt      /*define a variable based on an option.*/
         if @hasopt('k')  then k=min($.#+1, max(1, p(k 1)))
         if @hasopt('m')  then m=p(m 1)
         if @hasopt('d')  then dir=p(dir 1);
         return
 @put:    procedure expose $.;
            parse arg x,k;
            k=p(k $.#+1);
            call @parms 'k'
            $.@=subword($.@, 1, max(0, k-1))   x   subword($.@, k);
            call @adjust
         return
 
@show:   procedure expose $.;
            parse arg k,m,dir;
            if dir==-1  &  k==''   then k=$.#
            m=p(m $.#);
            call @parms 'kmd';
            say @get(k,m, dir);
         return
 
list:
/* this is only as examples how to use the double linked list
    for future expansion                                      */
  call sy 'initializing the list.'            ;  call @init
  call sy 'building list: blue'               ;  call @put "blue"
  call sy 'displaying list size.'             ;  say  "list size="@size()
  call sy 'forward list'                      ;  call @show
  call sy 'backward list'                     ;  call @show ,,-1
  call sy 'showing 4th item'                  ;  call @show 4,1
  call sy 'showing 5th & 6th items'           ;  call @show 5,2
  call sy 'adding item before item 4: black'  ;  call @put "black",4
  call sy 'showing list'                      ;  call @show
  call sy 'adding to tail: white'             ;  call @put "white"
  call sy 'showing list'                      ;  call @show
  call sy 'adding to head: red'               ;  call @put  "red",0
  call sy 'showing list'                      ;  call @show
return
 
readlex:
/* this reads the REXX lexicon into memory */
/* skippable stuff */
STMT_INCLUDE                    :   Include_Statement 
LINE_COMMENT                    :   Line_Comment_
BLOCK_COMMENT                   :   Block_Comment_
WHISPACES                       :   Whitespaces_   
CONTINUATION                    :   Continue_    

/* Keywords        */
KWD_ADDRESS                     :   A D D R E S S ;
KWD_ARG                         :   A R G ;
KWD_BY                          :   B Y ;
KWD_CALL                        :   C A L L ;
KWD_DIGITS                      :   D I G I T S ;
KWD_DO                          :   D O ;
KWD_DROP                        :   D R O P ;
KWD_ELSE                        :   E L S E ;
KWD_END                         :   E N D ;
KWD_ENGINEERING                 :   E N G I N E E R I N G ;
KWD_ERROR                       :   E R R O R ;
KWD_EXIT                        :   E X I T ;
KWD_EXPOSE                      :   E X P O S E ;
KWD_EXTERNAL                    :   E X T E R N A L ;
KWD_FAILURE                     :   F A I L U R E ;
KWD_FOR                         :   F O R ;
KWD_FOREVER                     :   F O R E V E R ;
KWD_FORM                        :   F O R M ;
KWD_FUZZ                        :   F U Z Z ;
KWD_HALT                        :   H A L T ;
KWD_IF                          :   I F ;
KWD_INTERPRET                   :   I N T E R P R E T ;
KWD_ITERATE                     :   I T E R A T E ;
KWD_LEAVE                       :   L E A V E ;
KWD_NAME                        :   N A M E ;
KWD_NOP                         :   N O P ;
KWD_NOVALUE                     :   N O V A L U E ;
KWD_NUMERIC                     :   N U M E R I C ;
KWD_OFF                         :   O F F ;
KWD_ON                          :   O N ;
KWD_OPTIONS                     :   O P T I O N S ;
KWD_OTHERWISE                   :   O T H E R W I S E ;
KWD_PARSE                       :   P A R S E ;
KWD_PROCEDURE                   :   P R O C E D U R E ;
KWD_PULL                        :   P U L L ;
KWD_PUSH                        :   P U S H ;
KWD_QUEUE                       :   Q U E U E ;
KWD_RETURN                      :   R E T U R N ;
KWD_SAY                         :   S A Y ;
KWD_SCIENTIFIC                  :   S C I E N T I F I C ;
KWD_SELECT                      :   S E L E C T ;
KWD_SIGNAL                      :   S I G N A L ;
KWD_SOURCE                      :   S O U R C E ;
KWD_SYNTAX                      :   S Y N T A X ;
KWD_THEN                        :   T H E N ;
KWD_TO                          :   T O ;
KWD_TRACE                       :   T R A C E ;
KWD_UNTIL                       :   U N T I L ;
KWD_UPPER                       :   U P P E R ;
KWD_VALUE                       :   V A L U E ;
KWD_VAR                         :   V A R ;
KWD_VERSION                     :   V E R S I O N ;
KWD_WHEN                        :   W H E N ;
KWD_WHILE                       :   W H I L E ;
KWD_WITH                        :   W I T H ;

/* Brackets  */
BR_O                            :   Br_O_ ;
BR_C                            :   Br_C_ ;

/* Special variables: RC, RESULT, SIGL  */
SPECIAL_VAR                     :   R C
                                |   R E S U L T
                                |   S I G L
                                ;
/* Label, const, var, number */
NUMBER                          :   Number_ ;
CONST_SYMBOL                    :   Const_symbol_ ;
VAR_SYMBOL                      :   Var_Symbol_ ;

/* String and concatenation */
STRING                          :   String_

/* Single characters */
fragment Stop_                  :   '.' ;
fragment Comma_                 :   ',' ;
fragment Colon_                 :   ':' ;
fragment Scol_                  :   ';' ;
fragment Eq_                    :   '=' ;
fragment Plus_                  :   '+' ;
fragment Minus_                 :   '-' ;
fragment Caret_                 :   '^' ;
fragment Logical_Not_           :   '¬' ;
fragment Underscore_            :   '_' ;
fragment Exclamation_mark_      :   '!' ;
fragment Question_mark_         :   '?' ;
fragment Br_O_                  :   '(' ;
fragment Br_C_                  :   ')' ;
fragment Space_                 :   ' ' ;
fragment Form_Feed_             :   '\f' ;
fragment HTab_                  :   '\t' ;
fragment VTab_                  :   '\u000b' ;
fragment Caret_Return_          :   '\r' ;
fragment New_Line_              :   '\n' ;
fragment Quote_                 :   '"' ;
fragment Apostrophe_            :   '\'' ;
fragment Slash_                 :   '/' ;
fragment Backslash_             :   '\\' ;
fragment Asterisk_              :   '*' ;
fragment More_                  :   '>' ;
fragment Less_                  :   '<' ;
fragment Percent_sign_          :   '%' ;
fragment VBar_                  :   '|' ;
fragment Amp_                   :   '&' ;
fragment Hash_                  :   '#' ;
fragment At_                    :   '@' ;
fragment Dollar_                :   '$' ;

/* Letters */
fragment A                      :   ('a'|'A');
fragment B                      :   ('b'|'B');
fragment C                      :   ('c'|'C');
fragment D                      :   ('d'|'D');
fragment E                      :   ('e'|'E');
fragment F                      :   ('f'|'F');
fragment G                      :   ('g'|'G');
fragment H                      :   ('h'|'H');
fragment I                      :   ('i'|'I');
fragment J                      :   ('j'|'J');
fragment K                      :   ('k'|'K');
fragment L                      :   ('l'|'L');
fragment M                      :   ('m'|'M');
fragment N                      :   ('n'|'N');
fragment O                      :   ('o'|'O');
fragment P                      :   ('p'|'P');
fragment Q                      :   ('q'|'Q');
fragment R                      :   ('r'|'R');
fragment S                      :   ('s'|'S');
fragment T                      :   ('t'|'T');
fragment U                      :   ('u'|'U');
fragment V                      :   ('v'|'V');
fragment W                      :   ('w'|'W');
fragment X                      :   ('x'|'X');
fragment Y                      :   ('y'|'Y');
fragment Z                      :   ('z'|'Z');

return
