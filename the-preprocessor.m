!! These replacements might help with memory issues
replace indexOfOutsideQuotes:qiooq
replace incrementLineNumber:qiln

endl$ = chr$(13) + chr$(10)

function indexOfOutsideQuotes(start, str$, search$) {
  x = instr(start, str$, search$)
  temp$=''
  character$=''
  curQuote%=0
  if (x>0) {
    for i=1 to len(str$) {
      character$ = mid(str$, i, 1)
      if character$='"' {
        curQuote%=-1 if curQuote%=1
        curQuote%=1 if curQuote%=0
        curQuote%=0 if curQuote%=-1
      }
      if character$="'" {
        curQuote%=-1 if curQuote%=2
        curQuote%=2 if curQuote%=0
        curQuote%=0 if curQuote%=-1
      }
      temp$ = temp$ + character$ if curQuote%=0
      temp$ = temp$ + chr$(0) if curQuote%=0
    }
    assign instr(start, temp$, search$)
  }
  else {
    assign 0
  }
}

function takeAllButLastAndAddCRLF$(na$) {
  assign LEFT(NA$,LEN(NA$)-1%)+endl$
}

function handleLeftBracketLine() {
  iY=1
  while 0%=0% {
    iL$=CVT$$(A$,444%)
    iA=INSTR(iY,iL$,'[')
    break if ia=0
    iB$=CVT$$(A$,508%)  ! converts [ to (
    iB=INSTR(iA,iB$,'(')
    IF iB=iA AND IB>0 {
      A$=LEFT(iL$,iA-1)
      break if 0%=0%
    }
    break IF iA=0 
    iY=iA+1
  }
}

function processReplacements$(x$) {
  if (nr>0) {
    FOR TT=0 TO NR-1 {
      firstTime=1
      HU=0
      GB=1
      while (hu<>0 or firstTime=1) {
        firstTime=0
        K$=CVT$$(x$,444%)+endl$
        HU=INSTR(HU+GB,K$,SR$(TT))
        if (hu<>0) {
          JL$=LEFT(K$,HU-1)
          er$=RIGHT(K$,HU+LEN(SR$(TT)))
          x$=JL$+NR$(TT)+ER$+endl$
          GB=len(nr$(tt))-len(sr$(tt))+1
          gb=1 IF GB<1
        }
        er$=''
        jl$=''
        k$=''
      }
    }
  }
  assign x$
}

function lineNumber$() = NUM1$(L(C))

function incrementLineNumber() {
  l(c) = l(c) + 1
}

function getNextLine$() {
  procedure whenError {
    if inFile%=1 {
      if err<>11 {
        print sys(chr$(6)+chr$(9)+chr$(err))
      }

      PRINT #9%,'2999 GOTO 32767'
      PRINT #9%,'32767 END'
      CLOSE #1%,#9%
      PRINT "Compiling time:";TIME(0)-BW;"seconds"
      CHAIN left(s$,instr(1,s$,'.')-1)+'.OBM' IF runAfterCompiling=1
      end
    }
    else {
      CLOSE #2:inFile%=1
      resume
    }
  }
  set error whenError

  INPUT LINE #inFile%,nextLine$
  assign nextLine$  
}

function processBlock(label$) {
  C=C+1

  if len(label$)>0 {
    PRINT #9%,lineNumber$();' !!! ';label$
    incrementLineNumber()
  }

  done(c)=0
  while done(c)=0 {
    a$ = getNextLine$()
    NA$=CVT$$(A$,444%)

    if INSTR(1,A$,'[')>0 {
      handleLeftBracketLine()
      a$ = na$+endl$
    }

    if right(NA$, len(na$))='{' {
      na$=left(na$, len(na$)-1)
    }

    if left(na$, 1)='}' {
      done(c) = 1
      PRINT #9%,lineNumber$();' assign'
      incrementLineNumber()
      C=C-1
      incrementLineNumber()
    }
    else {
      a$ = na$ + endl$
      / lineWasWritten = FNparseLine()

      if lineWasWritten<>1 {
        if inFunctionDef=1 and left(a$,6)="assign" {
          if len(a$)>6 {
             fuRV$=CVT$$(RIGHT(A$,7),444%)
             PRINT #9%,lineNumber$();' ';funcName$+"="+fuRV$;": assign"
             incrementLineNumber()
          }
          else {
             PRINT #9%,lineNumber$();' assign'
             incrementLineNumber()
          }
        }
        else {
          PRINT #9%,lineNumber$();' ';A$;
          incrementLineNumber()
        }
      }
    }
  }
}

function handleIf() {
  conditional$=CVT$$(RIGHT(A$,3),444%)
  elsClause$(C) = conditional$
  PRINT #9%, lineNumber$();' IF ';conditional$;' THEN GOSUB';L(C+1);':BRK=0'
  processBlock('')
  assign 1
}

function handleWhile() {
  conditional$=CVT$$(RIGHT(A$,6),444%)
  PRINT #9%,lineNumber$();' IF ';conditional$;' THEN GOSUB';L(C+1)
  incrementLineNumber()
  PRINT #9%,L(C);' IF BRK=0 AND ';conditional$;' THEN GOTO ';L(C)-1
  incrementLineNumber()
  print #9%,L(c);' BRK=0'
  processBlock('')
  assign 1
}

function handleFor() {
  K=INSTR(1,A$,' '): P=INSTR(1,A$,'='): V$=MID(A$,K,P-K)
  PRINT #9%,lineNumber$();' ';CVT$$(A$,444%);':GOSUB';L(C+1)
  incrementLineNumber()
  PRINT #9%,L(C);' IF BRK=1 THEN ';L(C)+2
  PRINT #9%,L(C)+1;' NEXT ';V$:L(C)=L(C)+2
  processBlock('')
  PRINT #9%,L(C);' BRK=0'
  assign 1
}

function handleElse() {
  PRINT #9%,lineNumber$();' IF NOT(';elsClause$(C);') THEN GOSUB';L(C+1);':BRK=0'
  processBlock('')
  assign 1
}

function handleFunction() {
  funcName$ = includePrefix$(inFile%) + right(a$,10)

  inFunctionDef=1
  params$="": repl$=funcName$
  lbLoc=instr(1,funcName$,'(')
  if lbLoc>0 {
    repl$=left(funcName$,lbLoc-1)
    params$=right(funcName$,lbLoc)
  }

  SR$(NR)=repl$
  NR$(NR)=repl$
  NR$(NR)="FN"+repl$ if left(repl$,2)<>"FN"
  funcName$=nr$(nr):NR=NR+1

  if instr(1,params$,'=')>0 {
    PRINT #9%,L(C);' DEF ';funcName$;params$
    incrementLineNumber()
  }
  else {
    PRINT #9%,L(C);' DEF ';funcName$;cvt$$(params$, 444);': GOSUB ';L(C+1);": FNEND"
    incrementLineNumber()
    processBlock('Function ' + repl$)
    inFunctionDef=0
  }
  assign 1
}

function handleVoidFunctionCall() {
  print #9%, L(C);' MINTERNAL.X%=';A$
  incrementLineNumber()
  assign 1
}

function handleEnd() {
  PRINT #9%,L(C);' GOTO 32767'
  assign 1
}

function handleMove() {
  P$=RIGHT(CVT$$(A$,447%),5)
  PRINT #9%,L(C);' PRINT FNMOV$';P$;';'
  assign 1
}

function handleGet() {
  T$=CVT$$(RIGHT(A$,5),444%)
  PRINT #9%,L(C);' ';T$;'=FNGET$'
  assign 1
}

function handleSetError(errorProc$) {
  FOR WE=1 TO V {
    LI=LN(WE) IF CVT$$(errorProc$,447%)=CVT$$(M$(WE),447%)
  }
  PRINT #9%,L(C);' ON ERROR GOTO ';L(C+1)
  L(C+1)=L(C+1)+1
  PRINT #9%,L(C+1)-1;' GOTO ';LI;':GOTO 32767'
  assign 1
}

function handleProcedure(procName$) {
  M$(V)=procName$
  LN(V)=L(C+1)
  V=V+1
  processBlock("Procedure " + procName$)
  L(C)=L(C)-1
  assign 1
}

function handleReplace() {
  SR$(NR)=MID(A$,9,INSTR(1,A$,":")-9)
  NR$(NR)=cvt$$(RIGHT(A$,INSTR(1,A$,":")+1),444%)
  NR=NR+1
  assign 1
}

function handleBreak(hh$) {
  PRINT #9%,L(C);' ';CVT$$(HH$,444%);' THEN BRK=1:assign'
  assign 1
}

function handleArgs$(p$) {
  while LEFT(CVT$$(A$,447%),1)<>'}' {
    a$ = getNextLine$()
    PRINT #9%,lineNumber$();' ';A$
    incrementLineNumber()
  }
  incrementLineNumber()
  assign RIGHT(P$,2)
}

function handleCall(ee$, p$) {
  p$ = handleArgs$(p$) IF LEFT(P$,1)='&'
  FOR WE=1 TO V {
    LI=LN(WE) IF CVT$$(P$,447%)=CVT$$(M$(WE),447%)
  }
  PRINT #9%,L(C);EE$;LI
  incrementLineNumber()
  print #9%,l(c);' BRK=0'
  assign 1
}

function handleResume(continuePoint$) {
  PRINT #9%,L(C);' RESUME ';L(C)+1:assign
  IF CVT$$(continuePoint$,511)<>"" {
    incrementLineNumber()
    FOR WE=1 TO V {
      LI=LN(WE) IF CVT$$(P$,447%)=CVT$$(M$(WE),447%)
    }
    PRINT #9%,L(C);' GOSUB ';LI;':BRK=0'
  }
  assign 1
}

function handleBackslash() {
  zz=instr(1,a$,':')
  a$=right(a$,zz+1)
  l(c)=val(mid(a$,2,zz-2))
}

function replaceLeadingSlashes$(str$) {
  prepend$ = ''
  for i=1 until mid(str$, i, 1)<>'/' {
    i = i + 1
    prepend$ = prepend$ + '!'
  }
  assign prepend$ + right(str$, i)
}

function handleInclude(fileName$, prefix$) {
  fileName$ = cvt$$(fileName$,444%)
  fileName$=mid(fileName$,2,len(fileName$)-2)
  inFile%=2

  print "#including ";fileName$
  OPEN fileName$ for input as file #inFile%

  includePrefix$(inFile%)=''
  includePrefix$(inFile%)=prefix$+'.' if len(prefix$)>0

  assign 1
}

function handleIncludeAs(str$) {
  i = indexOfOutsideQuotes(1, str$, '=INCLUDE')
  prefix$ = left(str$, i-1)
  fileName$ = right(str$, i+8)
  assign handleInclude(fileName$, prefix$)
}

function parseLine() {
  UU=0  
  A$=CVT$$(A$,444%)+endl$
  IF LEFT(A$,1)='{' {
    A$=""
    UU=1
  }

  a$=replaceLeadingSlashes$(a$) if len(a$)>=2 and left(a$,2)='//'

  a$=processReplacements$(a$) IF LEFT(A$,1)<>'/' and left(a$,1)<>"}"

  A$=CVT$$(RIGHT(A$,2),444%)+endl$ IF LEFT(A$,1)='/'

  noSpaces$ = cvt$$(a$, 447%)

  handleBackslash() if left(a$,1)=chr$(92)   !!! backslash

  uu = handleIf() IF LEFT(A$,2)='IF'
  uu = handleWhile() IF LEFT(A$,5)='WHILE'
  uu = handleFor() IF LEFT(A$,3)='FOR'
  uu = handleFunction() if left(a$,8)='FUNCTION'
  uu = handleVoidFunctionCall() if left(a$,2)='FN'
  uu = handleElse() IF LEFT(A$,4)='ELSE'
  uu = handleEnd() IF noSpaces$='END'
  uu = handleMove() IF LEFT(A$,4)='MOVE'
  uu = handleGet() IF LEFT(A$,3)='GET'
  uu = handleSetError(RIGHT(A$,11)) IF LEFT(A$,9)='SET ERROR'
  uu = handleProcedure(RIGHT(A$,11)) IF LEFT(A$,9)='PROCEDURE'
  uu = handleInclude(RIGHT(A$,10), '') if LEFT(A$,8)='#INCLUDE'
  uu = handleIncludeAs(noSpaces$) if instr(1, noSpaces$, '=INCLUDE')>0 and 0<>indexOfOutsideQuotes(1, noSpaces$, '=INCLUDE')
  uu = handleReplace() IF LEFT(A$,7)='REPLACE'
  uu = handleBreak(RIGHT(A$,7)) IF LEFT(A$,5)='BREAK'

  uu = handleCall(' GOSUB ', right(a$,2)) IF LEFT(A$,1)='&'
  uu = handleCall(' GOTO ', right(a$,2)) IF LEFT(A$,1)='@'

  uu = handleResume(right(a$,7)) IF LEFT(A$,7)='RESUME ' and len(cvt$$(right(a$,7),447%))>0

  !!! blank lines or lines with comments only get skipped
  if len(noSpaces$)=0 or left(noSpaces$,1)='!' {
    l(c)=l(c)-1
    uu = 1
  }

  A$='INPUT #12%,'+RIGHT(A$,8) IF LEFT(A$,6)='INPUTZ'
  A$='INPUT LINE #12%,'+RIGHT(A$,7) IF LEFT(A$,5)='ILINE'
  A$='PRINT:PRINT '+RIGHT(A$,8) IF LEFT(A$,6)='PRINTL'

  incrementLineNumber() IF UU=1
  assign uu
}

function main() {
  V=1

  print '!!!!! ICE preprocessor - translates from M to BASIC-PLUS !!!!!'
  print '!!!!! By C.B.Council   V.1.7   1/16/88 !!!!!'
  print

  s$=SYS(CHR$(7))

  DIM L(31%),elsClause$(30%),FV$(50%),SR$(50%),NR$(50%),M$(50%),LN(50%)

  input 'File:';s$

  runAfterCompiling = 1
  skipRunFlag = INSTR(1,S$,'/NR')
  if skipRunFlag>0 {
    S$=LEFT(S$,skipRunFlag-1)
    runAfterCompiling = 0
  }

  print 'Now compiling...'
  BW=TIME(0)

  L(F)=2000%*(F+1) for f=2% to 31%
  L(1)=100%
  C=0%

  inFile% = 1%
  open s$ for input as file #1%
  open left(s$,instr(1,s$,'.')-1)+'.OBM' for output as file #9%

  print #9%, '1 EXTEND:RANDOM'
  print #9%, '2 OPEN "KB:" FOR INPUT AS FILE #12%'
  processBlock('Main entry point') while 0%=0%
}

main()
