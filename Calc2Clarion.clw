
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
destruct                PROCEDURE        
                    END

QVars       QUEUE     
VarName         cstring(25)
Calcvars        &calcvarsclass
calresult       string(255)
Calcpreviousvalue   string(255)       
            END

Calc2Expression  &Icalcscript 
Debugfilename   cstring(255)
Expressionstr   cstring(255)
CTR LONG    
    CODE
        
     Calc2Expression  &=  Calccreatescript() 
        Debugfilename = 'Calc2 debug.txt'
       ! Calc2Expression.logTo(Debugfilename)
        
    loop ctr = 1 to 50
           
        qvars.VarName = 'Var'&ctr
        qvars.Calcvars &= new(calcvarsclass)
        add(qvars)
        
         Calc2Expression.bindStringIfc(qvars.VarName,qvars.Calcvars.ICgetSetCString)   ! Bind class to calc 2
        put(qvars)
        
    END    

        Loop ctr = 1 to 50
            get(QVars,Ctr)
          Expressionstr = 'Var1 = 1 * 2 ;'
          qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
          put(QVars)
        
          if CTR > 1
             Expressionstr = 'Var'&CTR&' = 2 * Var'&CTR-1&' ;'
             qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
                put(QVars)
                
                
                Expressionstr = 'Var'&CTR-1&';'
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
                                '~@s25@#1#255L(2)|M~Result~@n25.2@#3#255L(2)|M~Previous Value~@n' & |
                                '25.2@#4#')
                        END


    CODE
        
    open(Window)


    ACCEPT
        
    END
    close(Window)

    
calcvarsclass.destruct                PROCEDURE        

    CODE
    if not self.cstringref &= NULL    
       dispose(self.cstringref)
    END
    
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
areturnvalue  cstring(255)   
    CODE
     areturnvalue =  self.Varany
     return  areturnvalue 
        
calcvarsclass.ICgetSetCString.set                              PROCEDURE (const *cstring char)! , raw ! set cstring 
    CODE       
    self.Varany = char        