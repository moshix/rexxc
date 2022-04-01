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
/*  V0.5    :  Make lexical rules an outside file                    */
 
 
/* configuraiton parameters - IMPORTANT                              */
version="0.5"              /*                                        */
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
call generateparser      /* read in lexicon into memory                          */
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
 

readsource:
/* ------------------------------------------------------------
 * Read REXX File
 * ------------------------------------------------------------
 */
tk=open("'PEJ.EXEC(BITA)'","R")
stream=''
do until eof(tk)
   stream=stream||read(tk)';'    /* fetch all lines in one big string */
end
si=0
string=stream
return


parse:
/* ------------------------------------------------------------
 * Process String
 * ------------------------------------------------------------
 */
do until string=''
   si=si+1
   stmt=instruction(string)   /* Get next instruction */
   if strip(stmt)=''          then iterate
   else if dostmt(stmt)  =1   then say "DO discovered     : '"stmt"'"
   else if setstmt(stmt) =1   then say "SET discovered    : '"stmt"'"
   else if ifstmt(stmt)  =1   then say "IF  discovered    : '"stmt"'"
   else if callstmt(stmt)=1   then say "CALL discovered   : '"stmt"'"
   else if endstmt(stmt) =1   then say "END discovered    : '"stmt"'"
   else if saystmt(stmt) =1   then say "SAY discovered    : '"stmt"'"
   else if returnstmt(stmt)=1 then say "RETURN discovered : '"stmt"'"
   else if exitstmt(stmt)=1   then say "EXIT discovered   : '"stmt"'"
   else say '++++ unknown "'stmt"'"
end
return


/* ------------------------------------------------------------
 * Get Next Instruction
 * ------------------------------------------------------------
 */
instruction:
   parse value arg(1) with statement';'string
return statement
/* ------------------------------------------------------------
 * Test for DO Instruction
 * ------------------------------------------------------------
 */
dostmt:
   parse upper arg keyw
   if word(keyw,1)<>'DO' then return 0
   ww1=words(keyw)
   if ww1=1 then do
      return 1
      say '*** simple DO Loop'
   end
   ww1=word(keyw,2)
   if ww1='FOREVER' then do
      return 1
      say '*** DO FOREVER'
   end
   if ww1='WHILE' then do
      parse value keyw with kdo' 'kwhile' 'xwhile
      return 1
      say '*** DO WHILE 'xwhile
   end
   if ww1='UNTIL' then do
      parse value keyw with kdo' 'kwhile' 'xwhile
      return 1
      say '*** DO UNTIL 'xwhile
   end
return 1
/* ------------------------------------------------------------
 * Test for SET Instruction
 * ------------------------------------------------------------
 */
setstmt:
   parse upper arg instmt
   if pos("=",instmt)=0 then return 0
   parse value instmt with before"="after
   ww1=words(before)
   if ww1=0 | ww1>1 then return 0
return 1
/* ------------------------------------------------------------
 * Test for IF Instruction
 * ------------------------------------------------------------
 */
ifstmt:
   parse upper arg instmt
   if word(keyw,1)<>'IF' then return 0
return 1
/* ------------------------------------------------------------
 * Test for CALL Instruction
 * ------------------------------------------------------------
 */
callstmt:
   parse upper arg keyw
   if word(keyw,1)<>'CALL' then return 0
return 1
/* ------------------------------------------------------------
 * Test for END Instruction
 * ------------------------------------------------------------
 */
endstmt:
   parse upper arg keyw
   if word(keyw,1)<>'END' then return 0
return 1
/* ------------------------------------------------------------
 * Test for SAY Instruction
 * ------------------------------------------------------------
 */
saystmt:
   parse upper arg keyw
   if word(keyw,1)<>'SAY' then return 0
return 1
/* ------------------------------------------------------------
 * Test for RETURN Instruction
 * ------------------------------------------------------------
 */
returnstmt:
   parse upper arg keyw
   if word(keyw,1)<>'RETURN' then return 0
return 1
/* ------------------------------------------------------------
 * Test for EXIT Instruction
 * ------------------------------------------------------------
 */
exitstmt:
   parse upper arg keyw
   if word(keyw,1)<>'EXIT' then return 0
return 1

