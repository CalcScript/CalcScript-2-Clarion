 ! Calc Interface - (c) copyright stephen ryan , allan turner 2016
 !typedef enum 
 
 !   {c:\win32claclasses\Icalc_interface.inc
!   SOME_NULL,
!   SOME_INTEGER,
!   SOME_NUMBER,
!   SOME_STRING,
!//   SOME_ARRAY,
!   SOME_BOUND,
!//   SOME_MAP,
!//   SOME_REF
!}typ_t;
  omit('******')
 
extern "C" calc_interface *createCalcInterface()
{
    return new calc();
}

extern "C" calc_interface *createChildCalcInterface(calc_interface *parent)
{
    return new calc((calc *)parent);
}

extern "C" void destroyCalcInterface(calc_interface *ci)
{
   delete (calc *)ci;
    }

class calc_interface;
class Bindable;
typedef void __stdcall func_t(calc_interface *ifc);

class getSetInt
{
public:
	virtual int32_t __stdcall get(void) = 0;
	virtual void   __stdcall set(int32_t i) = 0;
};

class getSetNumber
{
public:
	virtual float64_t __stdcall get(void) = 0;
	virtual void   __stdcall set(float64_t d) = 0;
};

class getSetString
{
public:
	virtual char * __stdcall get(void) = 0;
	virtual void   __stdcall set(char *str) = 0;
};

class getSetObject
{
public:
    virtual void  __stdcall setMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall getMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall invokeMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall removeMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall cleanUp(calc_interface *interface, char *memberName) = 0;
    virtual char * __stdcall typeName() = 0;
};

/* Underlying class used for all bindings */
class Bindable
{
public:
	virtual ~Bindable(){}
    virtual void  __stdcall get(calc_interface *interface) = 0;
    virtual void  __stdcall set(calc_interface *interface) = 0;
    virtual void  __stdcall invoke(calc_interface *interface) = 0;
    virtual void  __stdcall cleanUp(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall getMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall setMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall invokeMember(calc_interface *interface, char *memberName) = 0;
    virtual void  __stdcall	removeMember(calc_interface *interface, char *memberName) = 0;
    virtual char * __stdcall typeName() = 0;
};


class calc_interface
{
public:
	virtual tokenHandle __stdcall tokenize(char *str) = 0;
	virtual void  __stdcall freeHandle(tokenHandle handle) = 0;
	virtual	int32_t __stdcall computeInt(tokenHandle handle) = 0;
	virtual	float64_t  __stdcall computeNumber(tokenHandle handle) = 0;
	virtual	char * __stdcall computeStr(tokenHandle handle) = 0;

	virtual int32_t  __stdcall computeIntFromString(char *str) = 0;
	virtual float64_t  __stdcall computeNumberFromString(char *str) = 0;
	virtual char * __stdcall computeStrFromString(char *str) = 0;

	virtual int  __stdcall getError(void) = 0;
	virtual const char * __stdcall getErrorMesg(void) = 0;

/* base bindings */
	virtual void  __stdcall bindInteger(const char *n, int32_t *v) = 0;
	virtual void  __stdcall bindNumber(const char *n, float64_t *v)= 0;
	virtual void  __stdcall bindString(const char *n, char *v, int l) = 0;
	virtual void  __stdcall bindBindable(const char *n, Bindable *b, void *context) = 0;
	virtual void  __stdcall bindevents(const char *n, calcevents *addr, void *context) = 0;
	virtual void  __stdcall bindeventsint(const char *n, calcevents *addr, int32_t context) = 0;
	virtual void  __stdcall binddelegate(const char *n, calcevents *calceventsdelegate, void *context) = 0;
	virtual void  __stdcall binddelegateint(const char *n, calcevents *calceventsdelegate, int32_t context) = 0;
	virtual void  __stdcall bindiobjectmethod(const char *n,byte bindabletype, Bindable *objectmethoddelegate) = 0;

	/*  lazy loading function or variables */

	virtual void  __stdcall bindLazyFunction(void(*f)(char *lazyName))=0;

/* helper bindings */
	virtual void  __stdcall bindFunction(const char *n, func_t *f, void *context)= 0;
	virtual void  __stdcall bindIntegerIfc(const char *n, getSetInt *i) = 0;
	virtual void  __stdcall bindNumberIfc(const char *n, getSetNumber *i)= 0;
	virtual void  __stdcall bindStringIfc(const char *n, getSetString *i) = 0;
	virtual void  __stdcall bindObjectIfc(const char *n, getSetObject *i, void *context) = 0;
	virtual void  __stdcall bindObjectIfcint(const char *n, getSetObject *i, int32_t context) = 0;
	

/* advanced interface not normally needed/used */
	virtual void  __stdcall compute(tokenHandle handle) = 0;
	virtual void  __stdcall computeRestart(void) = 0;
	virtual void  __stdcall clrAllocStr() = 0;
	
/* For advanced use
 * Use compute() to do the calc
 * Use mathType() to get the type the calculation results in
 * And use the appropriate mathPop to get the result
 * Note. For very advanced use this can be done more than once where the calculation is 
 * expected to return more than one result
 */
	virtual typ_t  __stdcall mathType(void) = 0;
	virtual int32_t  __stdcall mathPopInt(void)= 0;
	virtual float64_t  __stdcall mathPopNumber(void)= 0;
	virtual char * __stdcall mathPopStr(void) = 0;
	virtual Bindable * __stdcall mathPopBind(void) = 0;
 
/* Methods for use in bindings */
	virtual void * __stdcall getBoundContext(void) = 0;
	virtual int32_t __stdcall getBoundContextint(void) = 0;
	virtual char * __stdcall getBoundName(void) = 0;
	virtual void __stdcall getBoundInfo(void **Pname, void **Ctxt, int32_t *Pcount) = 0;
	
/* parameters to bindings */
	virtual int  __stdcall getParamCount(void) = 0;
	virtual typ_t __stdcall getParamType(int p) = 0;

	virtual int32_t  __stdcall getParamInteger(int p) = 0;
	virtual float64_t  __stdcall getParamNumber(int p) = 0;
	virtual void __stdcall getParamStringByRef(int p, int dimLen, char *str) = 0;
	virtual Bindable *__stdcall getParamBind(int p) = 0;
	virtual BOOLEAN __stdcall getParamBindaddress(int p,int32_t  *d,byte *t) = 0; // get the address and type of the bound bindable or event delegate

	virtual char * __stdcall getParamStr(int p) = 0;

/* set the return value from bindings */
	virtual void  __stdcall setReturnInteger(int32_t l) = 0;
	virtual void  __stdcall setReturnNumber(float64_t d) = 0;
	virtual void  __stdcall setReturnString(char *s) = 0;
	virtual void  __stdcall setReturnBind(Bindable *b, void *context) = 0;
	virtual void  __stdcall setReturnSelfWithContext(void *context) = 0;
	virtual void  __stdcall setReturnSelfWithContextint(int32_t context) = 0;
	virtual void  __stdcall setReturnObjectIfc(getSetObject *o, void *context) = 0;
 
/* register a dictionary or hash table via a delete to list bindables 
  
//	virtual void setbindabletabledelegate() = 0;
//  virtual int getbindable() = 0;

	virtual int __stdcall calllogTo(char *message) = 0;
	virtual int __stdcall logTo(char *fileName) = 0;
 
};

! ******

 
Include('Iobjectaddress.inc'),once  

bindabletypeid      ITEMIZE,PRE    
datamember                 equate(0) ! not a bindable but a data member with no external binable or interface or delegate 
getsetinvokeobject         equate(1) ! invokeable with context 
invokeobject               equate(2) ! invoke no context
eventsdelegate             equate(3) ! event delegate
calcscriptfunction         equate(4) ! calc script function  method supported by direct calc call  
functiontype               equate(5) ! function invoke with interface only parameter , method is supported by direct calc call ,Invokemethod
objectbindable             equate(6) ! object bindable     
getsetstring               equate(7) ! get set string bindable 
getsetfloat                equate(8) ! get set float bindable
getsetint                  equate(9) ! get set int bindable
callbackclaptr             equate(10)   ! clarion call back interface, clarion cannot support *(void)          
clientinterface            equate(11)   ! client defined interface                         
                    END

! Calc script object support.

calcevent      ITEMIZE,PRE
set               equate(1)    ! set value, Var = var or value or function             
get               equate(2)    ! get a value, get a var or a member or some value in a list representing an entity of some kind
invoke            equate(3)    ! invoke ()
cleanup           equate(4)    ! clean up
              END

IEventsCallBacks    Interface   ! set event callbacks
SetEventTrue            PROCEDURE  ! call back on set events true
GetEventTrue            PROCEDURE  ! call back on get events true
InvokeTrue              PROCEDURE  ! call back on Invoke events true
SetEventFalse           PROCEDURE  ! call back on set events false
GetEventFalse           PROCEDURE  ! call back on get events false
InvokeFalse             PROCEDURE  ! call back on invoke events false
                    END



! IStacklist - manage stacklists   

IStackScopes  INTERFACE
newscope             PROCEDURE  ! create a new dyna list
newscopeassign       PROCEDURE  ! assign new list and reset vars 
reassign             PROCEDURE  ! assign popped list
Pushlist             PROCEDURE  ! push list 
Poplist              PROCEDURE  ! pop list
                END    

! to bind finction
! bind class 
!typedef             void __stdcall func_t(calc_interface *ifc);
! is func_t which will  be the address of a function that passes ICalc_interface_2

Invokemethod        INTERFACE
invoke                  procedure(long ADDRESSOfparameters) ! invoke
                    END

! bind calc script bind vars interface

Igetcalcscriptbindscope     INTERFACE
get                             procedure(),*IBindICalcscript   ! get a calcscript to bind to 
                            END

! IBind to calc

IBindICalcscript            INTERFACE
bindifc                        procedure(const *cstring ObjectNametxt,ICalcBindable ICalcBindableref,const *cstring datacontextname)
bindifcint                     procedure(const *cstring ObjectNametxt,ICalcBindable ICalcBindableref,long datacontextidNo)
bindIntegerIfc                 procedure(const *cstring n, ICgetSetInt  ICgetSetIntaddress) 
bindNumberIfc                  procedure(const *cstring n, ICgetSetReal ICgetSetRealaddress) 
bindStringIfc                  procedure(const *cstring n, ICgetSetCString ICgetSetCStringaddress) 
                            END

  ! ISet 

IsetIsetIBindICalcscript INTERFACE  ! set an IsetICalcscriptBindvars to assign to a client object
set                             procedure(IsetIBindICalcscript IsetIBindICalcscriptref)
                            END

IsetIBindICalcscript     INTERFACE  ! set the calc script bind vars interface, the client gets the instance of calc script to bind to.
set                             procedure(IBindICalcscript IBindICalcscriptref)
                            END

IgetIBindICalcscript     INTERFACE
get                             procedure(),*IBindICalcscript
                            END
    

ICgetSetInt       INTERFACE,com  ! Get set Integer C interface for Calc CPP runtime
get                 PROCEDURE(),proc, LONG  ! get interger
set                 PROCEDURE(long i)  ! set integer
                  END

ICgetSetReal     INTERFACE,com   ! Get set Integer C Float for Calc CPP runtime
get                PROCEDURE ,proc,*REAL   ! get float
set    	           PROCEDURE  (real d) ! set float or real, decimal
                 END

ICgetSetCString   INTERFACE,com  ! Get set Integer CString interface for Calc CPP runtime
get                  PROCEDURE (), *cstring   ! get cstring  
set                  PROCEDURE ( const *cstring char)   ! set cstring 
                 END
  
Igetpropertiesaddress     INTERFACE
getaddress                     procedure(),LONG ! get address
getaddresstype                 procedure(*long propertytype),long  ! get the address and type
                          END
 

IFunctionBindvar    INTERFACE  ! bind a string variable 
bindstring              procedure(*Cstring Var,ICgetSetCString ICgetSetCStringref)
                    END

IGlobalBindVar      INTERFACE(IFunctionBindvar)

                    END

IGlobalBindclass    INTERFACE    ! global bind class
bindclass                    procedure(const *cstring classname,ICalcBindable ICO,const *cstring context)  ! bind global class , context
                    END

 
! setup bindings to bind class to Calcscript Ifc ICalcscript_getSetObject

Ibindifc            INTERFACE
bindable               PROCEDURE
                    END
  
C_IdataContext        INTERFACE
getcontext              procedure,*cstring  ! get context data
                    END

IdataContext        INTERFACE
getcontext              procedure,string  ! get clarion specific context data as a string
                    END

! Calc interface


! Calc script class support interface to register your Classes with calcscript 2

ICalcscript_getSetObject     INTERFACE,COM
setMember                        PROCEDURE(*Icalcscript Icalcscriptref,const *cstring memberName) ,VIRTUAL
getMember                        PROCEDURE(*Icalcscript Icalcscriptref, const *cstring memberName),byte,virtual  ! note calc interface is return type void
invokeMember                     PROCEDURE(*Icalcscript Icalcscriptref, const *cstring memberName) ,VIRTUAL
removeMember                     PROCEDURE(*Icalcscript Icalcscriptref, const *cstring memberName)  ,VIRTUAL
cleanUp                          PROCEDURE(*Icalcscript Icalcscriptref, const *cstring memberName)  ,VIRTUAL
typeName                         PROCEDURE(), *cstring ,VIRTUAL
                                END

 
iclacalcscriptevents   INTERFACE  ! clarion specific interface for calc events , set, get, invoke and context name with dot as a space , Windows Controls Add   , Windows.Controls.Add()
events                    procedure(long event,*Icalcscript Icalcscriptref,string contextname) ,VIRTUAL ! calc script events and dyna list context 
geteventype               procedure(byte geteventype),byte ,virtual  ! get an event type status     
                        END
 
isetclacalcscriptevents     INTERFACE
set                             procedure(iclacalcscriptevents iclacalcscripteventsref)  ! set as call back for events
                            END

!IsetIcalcscript          INTERFACE
!set                         procedure(Icalcscript Icalcscriptref) ! set Icalcscript 
!                         END

IsetICalcscriptmembers        INTERFACE
set                             procedure(ICalcBindable ICalcBindableref) ! get ICalcscript_getSetObject interface
                             END
 

iobjectmethod       interface,com  ! call method with it a member or its name , IE use this to mimic calc 3's run a function 
callmethod              PROCEDURE(Icalcscript Icalcscriptref,byte bindabletype, const *cstring memberName),VIRTUAL 
                    END

ICalcBindable     interface,COM  ! Base calc class connector - use with setReturnObjectIfc, getParamBind 
getMember               PROCEDURE(Icalcscript Icalcscriptref, const *cstring memberName)  ,byte ,VIRTUAL
setMember               PROCEDURE(Icalcscript Icalcscriptref, const *cstring memberName) ,VIRTUAL
invokeMember            PROCEDURE(Icalcscript Icalcscriptref, const *cstring memberName)  ,VIRTUAL
removeMember            PROCEDURE(Icalcscript Icalcscriptref, const *cstring memberName) ,VIRTUAL
cleanUp                 PROCEDURE(Icalcscript Icalcscriptref, const *cstring memberName),VIRTUAL
typeName                PROCEDURE,*CSTRING,VIRTUAL
get                     PROCEDURE(Icalcscript Icalcscriptref) ,VIRTUAL
set                     PROCEDURE(Icalcscript Icalcscriptref) ,VIRTUAL
invoke                  PROCEDURE(Icalcscript Icalcscriptref) ,VIRTUAL
                    END

! calc script interface

Icalcscript   INTERFACE,COM
tokenize                    PROCEDURE(const *cstring text),proc,long,RAW  ! tokenise and return address of the tokens handle, a pre parser, use for expressions with multiple results, from 100 up to 10 million or more times
freeHandle                  PROCEDURE(long tokenshandle),RAW ! free the token list by its address returned from the method tokenise
computeInt                  procedure(long tokenshandle),proc,long , RAW ! compute an integer expression, with a handle to the pre parsed expression
computeDouble               procedure(long tokenshandle),proc,REAL  ,RAW ! compute an float or real expression, with a handle to the pre parsed expression
computeStr                  procedure(long tokenshandle),proc,cstring  ,RAW ! compute an string expression, with a handle to the pre parsed expression
computeIntFromString        procedure(const *cstring text) ,proc,long ,RAW  ! compute an integer expression with no pre parse speed up, use for single time 
computeDoubleFromString     procedure(const *cstring text) ,proc,REAL  ,RAW ! compute an float or real expressionwith no pre parse speed up, use for single time 
computeStrFromString        procedure(const *cstring text) ,proc,cstring  ,RAW! compute an string or real expressionwith no pre parse speed up, use for single time 
getError                    procedure,proc,long , raw ! get the returned error
getErrorMesg                procedure,proc,*cstring, raw ! get the returned error message
! Bind data 
bindInteger               procedure(const *cstring n, long number) ,RAW ! bind an integer as a variable
bindNumber                procedure(const *cstring n, long number),RAW ! bind a float as a variable
bindString                procedure(const *cstring n, const *cstring var, long varlength),RAW ! Bind a string as a variable
bindBindable              procedure(const *cstring n,long  FuncBindableaddress,const  *CSTRING datacontext),RAW  ! name of an object, address of object, data context is , anything to identify context 
! helper bindings
bindevents                PROCEDURE(const *cstring n, calcevents addr, const *CSTRING datacontext),raw ! bind an event procedure type of delegate 
bindeventsint             PROCEDURE(const  *cstring n, calcevents addr, long contextNumber),raw ! assing a typed function or delegate
binddelegate              PROCEDURE(const  *cstring n, calcevents addr,const *CSTRING datacontext),raw ! assign typed function or delegate , calc creates a bindable with a delegate ref and invokes
binddelegateint           PROCEDURE(const  *cstring n, calcevents addr, long contextNumber),raw ! assign typed function or delegate , calc creates a bindable with a delegate ref and invokes
bindiobjectmethod         PROCEDURE(const  *cstring n,byte bindabletypeid, ICalcBindable objectmethoddelegate),raw ! assign the delegate for a return bindable type  
!/*  lazy loading function or variables */
bindLazyFunction            PROCEDURE(long functionaddress) ,RAW   ! prototype  void(*f)(char *lazyName) 

! /* helper bindings */
bindFunction               procedure(const *cstring n, long functionaddress,*CSTRING context),RAW  ! name, function address, 
bindIntegerIfc             procedure(const *cstring n, ICgetSetInt ICgetSetIntaddress) ,RAW ! Bind an integer as an object
bindNumberIfc              procedure(const *cstring n, ICgetSetReal ICgetSetRealaddress),RAW ! bind a number as an object
bindStringIfc              procedure(const *cstring n, ICgetSetCString  ICgetSetCStringaddress),RAW ! bind a string as an object
bindObjectIfc              procedure(const *cstring n, ICalcBindable ICalcBindableref, const *CSTRING context) ,RAW ! returns a cstring address
bindObjectIfcint             procedure(const *cstring n, ICalcBindable ICalcBindableref, long context) ,RAW     ! returns a long for a row 
 
! Computer expressions
compute                     PROCEDURE(long tokensHandle) ,RAW ! Compute an expression , used by calc internally
computeRestart              PROCEDURE() ,RAW  ! Use to restart the compute expression resetting internal flags and buffers

clrAllocStr                 procedure() , RAW ! Internal 
! internal calls not for use externally
mathType                  procedure(),proc,byte , raw ! internal
mathPopInt                procedure( ),proc,LONG, raw ! internal
mathPopDouble             procedure( ),proc,real, raw ! internal
mathPopStr                procedure( ),proc,*cstring  , raw ! internal
mathPopBind               procedure,long, raw ! returns address of bindable

! get bound contexts
getBoundContext           PROCEDURE(),*CSTRING ,raw ! address of context   
getBoundContextint        PROCEDURE(),long,raw ! address of context or numeric, allows getting member object from a list 
getBoundName              PROCEDURE(),*cstring,RAW  !  calc name of object or function or variable or INTERFACE
getBoundInfo              PROCEDURE( *long Functionname,const *cstring Context, *long  Parametercount) ,RAW ! get infor of method or function execution

getParamCount           PROCEDURE() ,long ,RAW  ! get number of parameters in function call
getParamType            PROCEDURE(long ParameterNo),byte ,raw ! get parameter type of a function being invoked

! parameters to bindings */

getParamInteger         PROCEDURE(long parameterNo),long ,raw  ! get paramater integer
getParamNumber          PROCEDURE(long parameterNo),real,RAW  ! get parameter number
getParamStringByRef     PROCEDURE(long parameterNo, long  dimLen, const *cstring str),raw  ! get value from calcscript and put in local variable 
getParamBind            PROCEDURE(long parameterNo) ,*ICalcBindable,RAW ! get a base bindable object connector
getParamBindaddress     procedure(int p,*long d,*byte ),byte,raw ! get the address and type of the bound bindable or event delegate

getParamStr             PROCEDURE(long parameterNo),*cstring,RAW

! set the return value from bindings */

setReturnInteger            PROCEDURE(long l),RAW
setReturnNumber             PROCEDURE(REAL d),RAW
setReturnString             PROCEDURE(const *cstring str),RAW
setReturnBind               PROCEDURE(long  bindableaddress, *cstring contextdata),raw  
setReturnSelfWithContext    PROCEDURE(*cstring contextdata) ,raw 
setReturnSelfWithContextint procedure(long  context),raw 
setReturnObjectIfc          PROCEDURE(ICalcBindable ICalcBindableref, *cstring contextdata),raw 
calllogTo                   procedure(const  *cstring message),RAW
logTo                       PROCEDURE(const *cstring fileName),RAW
                    END

! Ibdebug  interface

IClaCalc2Debug      INTERFACE
logfilename             procedure(string logfilename)   
logmessage              procedure(string logmessage)
Setdebugon             PROCEDURE     
                    END

!  set Icalcscript interface

ISetIcalcscript     INTERFACE
set                     procedure(*Icalcscript  Icalcscriptref)  ! set an Icalcscript interface
                    END

IgetIcalcscript      INTERFACE
get                     procedure(),*Icalcscript ! get Icalcscript interface    
                      END

IAssignISetIcalcscript  INTERFACE  ! assign ISetIcalcscript interface
assign                      procedure(ISetIcalcscript ISetIcalcscriptref)
                        END


! calc script results

Igetsetcalcscriptresults  INTERFACE
getstring                    procedure,string  ! get string 
getreal                      procedure,REAL    ! get real 
getlong                      procedure,long    ! get long
setstring                    procedure(string valuestring)  ! get string 
setreal                      procedure(REAL valuereal)      ! get real 
gsetlong                     procedure(long valuelong)     ! get long
                          END

IsetIgetsetcalcscriptresults        INTERFACE
set                                     procedure(Igetsetcalcscriptresults Igetsetcalcscriptresultsref) ! set Igetsetcalcscriptresults interface
                                    END


! calc script macro values interface

Icalcscriptvalues            INTERFACE
getobjectname                       procedure,string  ! get object name
getcontextname                      procedure,string ! get context name
getobjectmembername                 procedure,string ! get object  member name
geteventidno                        procedure,long  ! get event id no
getDataexecutetype                  procedure(),byte  ! get Dataexecute type

getkeynamestr                       procedure(string keyname),proc,string ! get value for a key name
getvaluestr                         procedure(),proc,string ! get value in focus             
getstr                              procedure(),proc,string ! get value cstrng in focus             
getlong                             procedure(),proc,long ! get value long in focus             
getreal                             procedure(),proc,real ! get value real in focus             

setvaluestr                         procedure(string valueitem) ! set value as string                    
setvaluelong                        procedure(LONG valueitem) ! set value as long
setvaluereal                        procedure(REAL valueitem) ! set value as real                    
setcontextname                      procedure(string contextname) ! set context name
setobjectname                       procedure(string objectname) ! set object name
                                END


! interface to the calc script framework for clarion

ICalcClass               INTERFACE   !  object value with setgetobject callback interface ,byte contextonly dont call the interface but return context object 
addobject                      procedure(string objectkeyname,string contextname,byte interfacetype, ICalcBindable ICalcBindableref,<byte contextonly>),LONG ,proc! add object key , data context, option interface type and address 
addobjectmembers               procedure(string objectname,string keymembersname,byte interfacetype,ICalcBindable ICalcBindableref,<byte contextonly>),LONG ,proc! add object key, object and context name set as the same, optional interface to invoke
addobjectcode                   procedure(string objectname,string objectscriptcode) ! add an object script code
Addeventdelegate                procedure(string objectname,Icallbackfunction  Icallbackfunctionref) ! add a delegate or typed procedure address
Addeventdelegatemembers         procedure(string objectname,string keymembersname,Icallbackfunction Icallbackfunctionref) ! add a delegate or typed procedure address
addobjectmethod                 procedure(string objectkeyname,string contextname,byte interfacetype,ICalcBindable ICalcBindableref,<byte contextonly>),LONG ,proc! add object key , data context, option interface type and address 
                            END

IBaseClawindow      INTERFACE(Iobjectaddress)  ! base  clarion window interface
                    END

IClaControlproperty INTERFACE  ! get set a property
set                     procedure(string propertyname,*? propertyvalue) ! set value
get                     procedure(string propertyname),*? ! get property value
                    END

IcalcBindinterfaces     Interface
getglobalcalcscript       procedure(),*Icalcscript  ! get global calc script interface
geticalcscript             procedure(),*Icalcscript  ! get calc script in focus
bindscope                    procedure(),*Igetcalcscriptbindscope   ! get the binding interface for an objects context name , IE  Apps, Windows, Reports, UserData ect         
                        END

Idisposeobject      INTERFACE
disposeobject            procedure  ! dispose object
                    END

!Iobjectaddress      INTERFACE
!getaddress              procedure(),long,VIRTUAL  ! address
!                     END

! setup bindings to bind class to Calcscript Ifc ICalcscript_getSetObject

!Ibindifc            INTERFACE
!bindinterface          PROCEDURE(string ObjectName,ICalcBindable ICalcBindableref),VIRTUAL  ! bind an object name to interface
!bindable               PROCEDURE(string ObjectName),VIRTUAL  ! bind an object name 
!binddefaults            procedure,VIRTUAL  ! bind default objects   
!                    END

C_Ibindifc            INTERFACE
bindable               PROCEDURE(const *cstring ObjectName),VIRTUAL  ! bind an object name 
                        END



!IdataContext        INTERFACE
!getcontext              procedure,*cstring,VIRTUAL   ! get context data that determines the object path seperrated by dots  Functions.Source, Windows.Controls.add() ect 
!                    END

Icallcalc           INTERFACE   
Executecalc             procedure(string expression)  ! calc expression
                    END


C_Icallcalc           INTERFACE   
Executecalc             procedure(const *cstring expression)  ! calc expression
                        END


IClaCalcscriptContext     INTERFACE  ! Calc script context access
setMember             PROCEDURE(*Icalcscript Icalcscriptref,string memberName) 
getMember             PROCEDURE(*Icalcscript Icalcscriptref,string memberName)  
invokeMember          PROCEDURE(*Icalcscript Icalcscriptref,string memberName)
                            END
