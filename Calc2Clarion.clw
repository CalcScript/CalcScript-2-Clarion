
  PROGRAM

OMIT('***')
 * Created with Clarion 10.0
 * User: steve
 * Date: 29/11/2017
 * Time: 1:58 PM
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 ***

   ! Quick start clarion example using Calc 2 dll

   Include('Icalc_interface.inc'),ONCE
                   
                    MAP

                    module('Calcscript2')  !  Calc script 2 with object binding support

                            Calccreatescript  (), *Icalcscript, pascal, raw, name('createCalcscript')
                            Calcdestroyscript (*Icalcscript Iref), pascal, raw, name('destroyCalcscript')

                        END
    
    
                            calcwindow

                    END


calcvarsclass       class,TYPE,IMPLEMENTS(ICgetSetInt)|
                        ,IMPLEMENTS(ICgetSetReal)|
                        ,IMPLEMENTS(ICgetSetCString)

Varany                  ANY
VarName                 cstring(25)
cstringref              &cstring   ! return value to expression in c++ runtime
                    END

QVars       QUEUE     
VarName         cstring(25)
Calcvars        &calcvarsclass
calresult       string(255)
Calcpreviousvalue   LONG        
            END

Calc2Expression  &Icalcscript 
Debugfilename   cstring(255)
Expressionstr   cstring(255)
    CODE
        
     Calc2Expression  &=  Calccreatescript() 
        Debugfilename = 'Calc2 debug.txt'
        Calc2Expression.logTo(Debugfilename)
        
    loop ctr# = 1 to 10
           
        qvars.VarName = 'Var'&ctr#
        qvars.Calcvars &= new(calcvarsclass)
        add(qvars)
        
         Calc2Expression.bindStringIfc(qvars.VarName,qvars.Calcvars.ICgetSetCString)   ! Bind class to calc 2
        put(qvars)
        
    END    

    Loop ctr# = 1 to 10 
          get(QVars,Ctr#)
          Expressionstr = 'Var1 = 5 * 42 ;'
          qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
          put(QVars)
        
          if CTR# > 1
             Expressionstr = 'Var'&CTR#&' = 5 * Var'&CTR#-1&' ;'
             qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
                put(QVars)
                
                
                Expressionstr = 'Var'&CTR#-1&';'
                qvars.Calcpreviousvalue = Calc2Expression.computeIntFromString(Expressionstr)
                put(QVars)
                 
          END   
            
    END    
    Calcwindow
    
Calcwindow          PROCEDURE

Window                  WINDOW('Cacl 2'),AT(,,523,344),GRAY,FONT('Segoe UI',8,,FONT:regular)
                            BUTTON('&OK'),AT(419,313,41,14),USE(?OkButton),DEFAULT
                            BUTTON('&Cancel'),AT(461,313,42,14),USE(?CancelButton),STD(STD:Close)
                            LIST,AT(19,13,483,270),USE(?LIST1),FROM(qvars),FORMAT('53L(2)|M~Var Name' & |
                                '~#1#72L(2)|M~Result~#3#20L(2)|M~Previous Value~#4#')
                        END


    CODE
        
    open(Window)


    ACCEPT
        
    END
    close(Window)

calcvarsclass.ICgetSetInt.get             PROCEDURE() !,proc, LONG, raw ! get interger
    CODE
    return  self.Varany 
calcvarsclass.ICgetSetInt.set             PROCEDURE(long i) !, raw  ! set integer
    CODE               
     self.Varany = i
        
 ! Get set Integer C Float for Calc CPP runtime

calcvarsclass.ICgetSetReal.get             PROCEDURE! ,proc,*REAL, raw  ! get float
    CODE
    return (self.Varany) 
        
calcvarsclass.ICgetSetReal.set             PROCEDURE  (real d) !, raw  ! set float or real, decimal
    CODE
    self.Varany = d       

calcvarsclass.ICgetSetCString.get              PROCEDURE ()!,proc, *cstring  , raw! get cstring  
    CODE
    if Not self.cstringref &= NULL
       dispose(self.cstringref)
    END
    if not self.Varany &= NULL    
        self.cstringref  &= new(cstring(len(clip(self.Varany))+1)) ! data length
        self.cstringref = self.Varany 
     ELSE
        self.cstringref  &= new(cstring(1))   ! default length
        self.cstringref = '' 
    END 
   !     message('  get '&self.cstringref)
     return self.cstringref
        
calcvarsclass.ICgetSetCString.set                              PROCEDURE (const *cstring char)! , raw ! set cstring 
    CODE       
    self.Varany = char        