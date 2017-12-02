
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
    
                            
                            Calcwindow
                            Calcqueuewindow

                            ! Call clarions evaluate from calc script expressions

                            ClaEvaluate(*Icalcscript Calcinstance),name('ClaEvaluate') ,pascal ! Cla Evaluate  
    
                            ! Register functionality to Calc runtime

                            BindFunction(Const *Cstring FunctionName,Icalcscript IcRef,Long Funcaddress )
                            Clariondefaultfunctions(Icalcscript IcRef)
    
                            ! Queue name functions

                            Listwhocolumn	(queue q,long columnNo),proc,STRING 
                      

                    END


! Example Order List to access in scripts

Orderslist              QUEUE
OrderNo                     LONG
OrderDate                   long 
CustomerID                  LONG
NumberofStockpurchased      LONG
StockPrice                   REAL
Tax                         REAL
TotalOrder                  REAL
                        END

ISetGetString       INTERFACE
set                     procedure(clastring stringvalue)
get                     procedure,proc,Clastring 
                    END

Queuelists          QUEUE
QName                   string(25),name('QName')
q                       &Queue
ColsVarsList            &QueueColsVars
                    END

QueueColsVars       queue,TYPE
ColNo                   LONG
ColName                 string(55),name('ColName')
Colvarref               &ColvarsClass
                    END

! Cols ref Var Class to bind to Calc script

ColvarsClass            class,TYPE,IMPLEMENTS(ICgetSetInt)|
                        ,IMPLEMENTS(ICgetSetReal)|
                        ,IMPLEMENTS(ICgetSetCString)|
                        ,IMPLEMENTS(ISetGetString)

IcRef                   &Icalcscript 

Varref                       ANY
ColNo                       LONG
AddQueueColumn              procedure(long colnumber,queue q)    
construct                   PROCEDURE
destruct                    PROCEDURE
                        END

! Queue Manager - register queues with Calc

QueuesListClass         class,TYPE,IMPLEMENTS(ICalcBindable)
Queuelistsref               &Queuelists
IcRef                       &Icalcscript 
Objectcontext               cstring(55)
Bindname                    cstring(55)
AddQueue                    procedure(queue q,string QueueName)
construct                   PROCEDURE
destruct                    PROCEDURE
                        END    

! A Vars class - connect your clarion values to a CPP expression scripting engine

calcvarsclass       class,TYPE,IMPLEMENTS(ICgetSetInt)|
                        ,IMPLEMENTS(ICgetSetReal)|
                        ,IMPLEMENTS(ICgetSetCString)
    
IcRef                   &Icalcscript 
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
 
Expressionstr   cstring(255)
     
    CODE
    Calcqueuewindow 
     
    
Calcwindow          PROCEDURE

                    MAP
                        Calcnumberswithvars
                    END


Window                  WINDOW('Cacl 2'),AT(,,523,344),GRAY,FONT('Segoe UI',8,,FONT:regular)
                            BUTTON('&Calculate'),AT(18,308,41,14),USE(?OkButton),DEFAULT
                            BUTTON('&Cancel'),AT(461,313,42,14),USE(?CancelButton),STD(STD:Close)
                            LIST,AT(19,13,483,270),USE(?LIST1),FROM(qvars),FORMAT('53L(2)|M~Var Name' & |
                                '~@s25@#1#255L(2)|M~Result~@n25.2@#3#255L(2)|M~Previous Value~@n' & |
                                '25.2@#4#')
                            BUTTON('Queues'),AT(75,308),USE(?BUTTONQueues)
                        END


    CODE
        
    open(Window)


    ACCEPT
        case field()
        of ?OkButton
           case event() 
            of EVENT:Accepted
                
                Calcnumberswithvars
                
            END     
         of ?BUTTONQueues   
        case event() 
            of EVENT:Accepted
                
                Calcqueuewindow
                
            END
        
        End     
        
    END
    close(Window)

Calcnumberswithvars PROCEDURE

CTR1 LONG
    CODE
    if Calc2Expression &= NULL
        Calc2Expression  &=  Calccreatescript()
    END
        loop ctr1 = 1 to records(QVars)
        
            get(qvars,CTR1)
            if not qvars.Calcvars &= NULL
               dispose(qvars.Calcvars)    
            END    
        END    
        free(qvars)    
            
        !Debugfilename = 'Calc2 debug.txt'
       ! Calc2Expression.logTo(Debugfilename)
        
    loop ctr1 = 1 to 50
           
        qvars.VarName = 'Var'&ctr1
        qvars.Calcvars &= new(calcvarsclass)
        add(qvars)
        
         Calc2Expression.bindStringIfc(qvars.VarName,qvars.Calcvars.ICgetSetCString)   ! Bind class to calc 2
        put(qvars)
        
    END    

        Loop ctr1 = 1 to 50
            get(QVars,Ctr1)
          Expressionstr = 'Var1 = 1 * 2 ;'
          qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
          put(QVars)
        
          if CTR1 > 1
             Expressionstr = 'Var'&CTR1&' = 2 * Var'&CTR1-1&' ;'
             qvars.calresult = Calc2Expression.computeStrFromString(Expressionstr)    
                put(QVars)
                
                
                Expressionstr = 'Var'&CTR1-1&';'
                qvars.Calcpreviousvalue = Calc2Expression.computeIntFromString(Expressionstr)
                put(QVars)
                 
          END   
            
    END         
    
Calcqueuewindow     PROCEDURE

Calcexpressionsstring  cstring(1000)
 

Window                  WINDOW('Calc 2'),AT(,,523,344),GRAY,FONT('Segoe UI',8,,FONT:regular)
                            SHEET,AT(10,12,501,315),USE(?SHEET1)
                                TAB('Queues'),USE(?TAB1:2)
                                    BUTTON('Calc Some Queue Values'),AT(25,41),USE(?Calcsomequeuevalues)
                                    LIST,AT(26,68,452,239),USE(?LIST1),FROM(Orderslist), |
                                        FORMAT('59L(2)|M~Ordder No~71L(2)|M~Order Date~@d8@45L(2' & |
                                        ')|M~CustomerID~@n6@40L(2)|M~Qty~@n62@56L(2)|M~StockPric' & |
                                        'e~@n-12.2@36L(2)|M~Tax~@n6.2@25L(2)|M~TotalOrder~@n-15.2@')
                                END
                                TAB('Queues and Expressions'),USE(?TAB2)
                                    LIST,AT(27,144,124,171),USE(?LISTQueues),HVSCROLL,VCR,FROM(''), |
                                        FORMAT('25L(2)|M~Queues~')
                                    LIST,AT(169,144,133,171),USE(?LISTColumns),HVSCROLL,VCR,FROM(''), |
                                        FORMAT('33L(2)|M~Col~55L(2)|M~Name~')
                                    TEXT,AT(27,39,461,68),USE(?TEXT1)
                                    BUTTON('Calc Expression'),AT(26,117),USE(?BUTTONcalc)
                                END
                            END
                            BUTTON('&Close'),AT(457,10,42,14),USE(?CancelButton),STD(STD:Close)
                        END

QueuesListClassref  QueuesListClass
    
OrdersExpression        &Icalcscript
Preparsedhandle         long
debugfilename           cstring(255)
    CODE
    if OrdersExpression &= NULL
        OrdersExpression &=  Calccreatescript()
        
        Clariondefaultfunctions(OrdersExpression)
      
        Debugfilename = 'Ordersdebug.txt'
       OrdersExpression.logTo(Debugfilename)
      QueuesListClassref.IcRef &= OrdersExpression
        
     open(Window)
     ?LISTQueues{prop:from} = QueuesListClassref.Queuelistsref
     ?TEXT1{Prop:Use} = Calcexpressionsstring
      QueuesListClassref.AddQueue(Orderslist,'Orderslist')  
      calcexpressionsstring  = 'Queues.Set(''Orderslist'',''OrderNo'', 1+Queues.Count(''Orderslist''));Queues.Set(''Orderslist'',''OrderDate'',today());Queues.add(''Orderslist'');'
            
         
         
    ACCEPT
        case field()
        of ?BUTTONcalc
           case event() 
            of EVENT:Accepted
            if Preparsedhandle = FALSE
                Preparsedhandle = OrdersExpression.tokenize(Calcexpressionsstring)
                OrdersExpression.computeStr(Preparsedhandle)   
            ELSE
                OrdersExpression.computeStr(Preparsedhandle)   
                
            END  
            END
            
        of ?LISTQueues
           case event() 
            of EVENT:Newselection
                get(QueuesListClassref.Queuelistsref,choice())
                if not errorcode()
                    if not QueuesListClassref.Queuelistsref.ColsVarsList &= NULL
                      ?LISTColumns{Prop:From} = QueuesListClassref.Queuelistsref.ColsVarsList
                    END 
                END    
            END
            
        of ?Calcsomequeuevalues
           case event() 
            of EVENT:Accepted
                
                
                
           END     
        End     
        
    END
    close(Window)
   END
    
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
        
    
! Queue Manager - register queues with Calc
! Bind this class to calc script 
    

QueuesListClass.ICalcBindable.get                     PROCEDURE(Icalcscript Icalcscriptref) 
    Code 
    
QueuesListClass.ICalcBindable.set                     PROCEDURE(Icalcscript Icalcscriptref) 
    Code 
        
QueuesListClass.ICalcBindable.invoke                  PROCEDURE(Icalcscript Icalcscriptref)  
    Code     
    if not Icalcscriptref &= NULL 
             
    END    
   
    
QueuesListClass.ICalcBindable.setMember         PROCEDURE(*Icalcscript calcscriptref,const *cstring memberName) 
    CODE
     
QueuesListClass.ICalcBindable.getMember         PROCEDURE(*Icalcscript calcscriptref,const *cstring memberName) 

    CODE
    return(false)

QueuesListClass.ICalcBindable.invokeMember       PROCEDURE(*Icalcscript calcscriptref, const *cstring memberName) 

Lcommand  cstring(25)
Lp   string(25)
Lp1  string(25)
LP2  string(25) 
rtncontext  cstring(25)
    CODE
    if not calcscriptref &= NULL    
    self.Objectcontext = calcscriptref.getBoundName()
    Lcommand = memberName 
      ! get queue   
        case upper(self.Objectcontext)
        of 'QUEUES'
             
         case Upper(Lcommand)
         of 'ADD'
            lp1 = calcscriptref.getParamStr(0)
            
                self.Queuelistsref.QName = Lp1
                get(self.Queuelistsref,'QName')
                if errorcode()
                    add(self.Queuelistsref.q)
                 END
               
         of 'GET'
                self.Queuelistsref.QName = Lp1
                get(self.Queuelistsref,'QName')
                if not errorcode()
                
                END    
                    
         of 'SET'
                lp = calcscriptref.getParamStr(0)
              
                lp1 = calcscriptref.getParamStr(1)
                lp2 = calcscriptref.getParamStr(2)
                
                ! Calc Lp2
                
                message(' Lp2 '&Lp2)
                
                self.Queuelistsref.QName = upper(Lp) 
                get(self.Queuelistsref,'QName')
                if not errorcode()
                    self.Queuelistsref.ColsVarsList.ColName = upper(Lp1)
                    get(self.Queuelistsref.ColsVarsList,'ColName')
                    if not errorcode()
                       if not self.Queuelistsref.ColsVarsList.Colvarref &= null
                         self.Queuelistsref.ColsVarsList.Colvarref.ISetGetString.set(LP2) 
                        END 
                    ELSE
                        message(error()&' , Column Not found ,  '&Lp1)
                    END
                End    
        
         Of 'COUNT'       
                lp = calcscriptref.getParamStr(0)
              
                self.Queuelistsref.QName = upper(Lp) 
                get(self.Queuelistsref,'QName')
                if not errorcode()
                    if not self.Queuelistsref.q &= NULL
                        message(' r= '&records(self.Queuelistsref.q))
                      calcscriptref.setReturnInteger(records(self.Queuelistsref.q))      
                    END
                END        
          END     
       END      
    END
      
QueuesListClass.ICalcBindable.removeMember        PROCEDURE(*Icalcscript  calcscriptref, const *cstring memberName)  
    CODE
    if not calcscriptref &= NULL
     
        
    END     

QueuesListClass.ICalcBindable.cleanUp           PROCEDURE(*Icalcscript  calc_interface, const *cstring memberName)  

    CODE
    if not calc_interface &= NULL
    
    
    END  
    
QueuesListClass.ICalcBindable.typeName           PROCEDURE()  

    CODE
      return('')       
        
        
    
QueuesListClass.AddQueue                    procedure(queue q,string QueueName)
CTR LONG
cstrname  cstring(55)   
    CODE
    if not self.Queuelistsref &= NULL    
        self.Queuelistsref.QName = Upper(QueueName)
        get(self.Queuelistsref,'QName')
        if errorcode()
            self.Queuelistsref.QName = Upper(QueueName)
            self.Queuelistsref.q &= q  
            self.Queuelistsref.ColsVarsList &= new(QueueColsVars)
            add(self.Queuelistsref)    
            if not self.Queuelistsref.ColsVarsList &= NULL
               if not self.IcRef &= NULL
                  loop ctr = 1 to 100
                       if who(q,ctr) = ''
                          BREAK
                        ELSE 
                          self.Queuelistsref.ColsVarsList.ColNo = CTR  
                          self.Queuelistsref.ColsVarsList.Colvarref &= New(ColvarsClass)
                          self.Queuelistsref.ColsVarsList.ColName = upper(who(q,ctr)) 
                          add(self.Queuelistsref.ColsVarsList)
                          self.Queuelistsref.ColsVarsList.Colvarref.AddQueueColumn(Ctr,q)
                          self.Queuelistsref.ColsVarsList.Colvarref.Varref &= NULL
                          if self.Queuelistsref.ColsVarsList.Colvarref.Varref &= NULL  
                             self.Queuelistsref.ColsVarsList.Colvarref.Varref &= what(q,CTR)  
                          End
                      END      
                  END      
               END     
            END    
        END
        ! register your class
        if not self.IcRef &= NULL
           self.Bindname = 'queues'
           self.IcRef.bindObjectIfc(self.Bindname ,self.ICalcBindable,self.Bindname )
        END    
    END    
        
QueuesListClass.construct   PROCEDURE

    CODE
    self.Queuelistsref               &= new(Queuelists)
        

QueuesListClass.destruct    PROCEDURE

    CODE
    if not self.Queuelistsref  &= NULL    
       loop ctr# = 1 to records(self.Queuelistsref)    
             get(self.Queuelistsref,ctr#)
             if self.Queuelistsref.ColsVarsList &= NULL
                Loop ctrcolvars# = 1 to records(self.Queuelistsref.ColsVarsList)
                    get(self.Queuelistsref.ColsVarsList,ctrcolvars#)
                    if not self.Queuelistsref.ColsVarsList.Colvarref &= NULL
                       dispose(self.Queuelistsref.ColsVarsList.Colvarref) 
                    END
               END 
            END
            free(self.Queuelistsref.ColsVarsList)
            dispose(self.Queuelistsref.ColsVarsList)    
        END    
        
        dispose(self.Queuelistsref)
        free(self.Queuelistsref )
    END     
    
! Cols ref Var Class to bind to Calc script
 
ColvarsClass.construct                   PROCEDURE

    CODE
    self.Varref  &= NULL
        

ColvarsClass.destruct      PROCEDURE        

    CODE
    self.Varref &= NULL

ColvarsClass.AddQueueColumn              procedure(long colnumber,queue q)    
cstr  cstring(55)    
    CODE
    if not self.IcRef &= NULL 
        cstr = upper(who(q,colnumber))
        if not cstr = ''
           self.IcRef.bindStringIfc(cstr,self.ICgetSetCString)
        END 
   END         
    
ColvarsClass.ICgetSetInt.get             PROCEDURE() !,proc, LONG, raw ! get interger
    CODE
    return  self.Varref 
ColvarsClass.ICgetSetInt.set             PROCEDURE(long i) !, raw  ! set integer
    CODE               
     self.Varref = i
        
 ! Get set Integer C Float for Calc CPP runtime

ColvarsClass.ICgetSetReal.get             PROCEDURE! ,proc,*REAL, raw  ! get float
    CODE
    return (self.Varref) 
        
ColvarsClass.ICgetSetReal.set             PROCEDURE  (real d) !, raw  ! set float or real, decimal
    CODE
    self.Varref = d       

ColvarsClass.ICgetSetCString.get              PROCEDURE ()!,proc, *cstring  , raw! get cstring  
areturnvalue  cstring(255)   
    CODE
     areturnvalue =  self.Varref
     return  areturnvalue 
        
ColvarsClass.ICgetSetCString.set              PROCEDURE (const *cstring char)! , raw ! set cstring 
    CODE       
    self.Varref = char   


ColvarsClass.ISetGetString.set         procedure(clastring stringvalue)
    CODE
    self.Varref = stringvalue
      
    
ColvarsClass.ISetGetString.get          procedure 
    CODE         
    return self.Varref    
     
    
        
Listwhocolumn				procedure(queue q,long columnNo) 

	CODE
	if not q &= NULL
	   return(who(q,columnNo))	
	END
	return('')
        
        
BindFunction        PROCEDURE(Const *Cstring FunctionName,Icalcscript IcRef,Long Funcaddress )
evalfunctionname cstring(25)
        
    CODE
    if not IcRef &= NULL    
        evalfunctionname = FunctionName
        IcRef.bindFunction(FunctionName,Funcaddress,evalfunctionname)
    END    
        
Clariondefaultfunctions         procedure(Icalcscript IcRef)   ! set up default clarion functions
aprocaddress                        LONG 
evalfunctionname cstring(25)
    CODE
    if not IcRef &= NULL    
        aprocaddress = address(ClaEvaluate) 
        evalfunctionname = 'date' 
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'year' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'day' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'Month' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'InList' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'inrange' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'Choose' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'path' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'longpath' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'format' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
     evalfunctionname = 'deformat' 
       
        BindFunction(evalfunctionname,IcRef,aprocaddress)
    evalfunctionname = 'Clock' 
     
        BindFunction(evalfunctionname,IcRef,aprocaddress)
    evalfunctionname = 'Today' 
        BindFunction(evalfunctionname,IcRef,aprocaddress)
        
    END    

ClaEvaluate         PROCEDURE(*Icalcscript Calcinstance)   ! Cla Evaluate           
acresult        cstring(255)
strparameter    string(255)

Lparameters             QUEUE  ! List of parameters
lp                          string(255)  ! Parameter for evaluate
Lplen                       LONG ! length of parameter
                        END

PTotalLen   long  ! Total Parameter length

Evalstr  &STRING
i   LONG  ! counter
pn  LONG  ! Parameter number
r   long  ! parameter records
l   long  ! evak string parameters length 
pl  long  ! parameters length 

    CODE
     
     if not Calcinstance &= NULL 
      
            
        pn  =   Calcinstance.getParamCount() ! get parameters
        if pn  = 0  
            strparameter = Calcinstance.getBoundName()
            acresult = evaluate(Clip(strparameter)&'()')
            Calcinstance.setReturnString(acresult) ! set the result to its self just for fun
        else  
          loop  i = 0 to  pn  - 1 
                Lparameters.lp  = clip( Calcinstance.getParamStr(i) )                      
                Lparameters.Lplen = len(Lparameters.lp)        
                add(Lparameters)
                pl = pl + Lparameters.Lplen 
            end
            strparameter = Calcinstance.getBoundName() & '('
            r = records(Lparameters)
            l = (3 * r) - 1 ! parameter padding length
            pl =  pl + l + len(clip(strparameter)) + 2  ! parameters plus padding 
            Evalstr &= new(string(pl))
            Evalstr = strparameter 
            loop i = 1 to r
                get(Lparameters,i)
                 
                if i < r  ! last parameter add 2 characters
                  Evalstr = clip(Evalstr) & ''''& clip(Lparameters.lp)  &''','
                Else   
                 Evalstr = clip(Evalstr) & ''''& clip(Lparameters.lp)  &''')'
                End
                  
             END
            if r > TRUE
              acresult =  evaluate(Evalstr)
                Calcinstance.setReturnstring( acresult) ! set the result to its self just for fun
                if not Evalstr &= NULL
                  dispose(Evalstr)    
              End    
            End  
          end      
      end    
  
        