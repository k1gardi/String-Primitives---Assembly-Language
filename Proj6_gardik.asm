TITLE Designing low-level I/O procedures     (Proj6_gardik.asm)

; Author: Kaewan Gardi
; Last Modified: 12/05/2020
; OSU email address: gardik@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: Project 6                Due Date: 12/06/2020
; Description: The program introduces itself and the author, prints a description of the extra credit
;		implemented, then prints a description of the programs functionality.
;		The program then obtains 10 signed integers from the user.
;		The integers must fit inside a 32 bit register.  Each input is first validated to only
;		contain numerical values, the first character is allowed to be a sign character.  The string
;		input is then converted to an integer and saved to memory as an SDWORD.
;		The integer is appended to an array.  This is repeated until 10 integers are obtained and
;		the array is full.
;		The program then displays a message and prints back all valid integers input.
;		Each integer is converted into an ASCII string before being printed by WriteVal
;		The sum of the array elements is then calculated and displayed with a label message.
;		This sum will be used to calculate and display an average value of the elements with a label message.
;		Both the sum and average are saved to memory.
;		Finally, a farewell message prints.
;		
;		Extra Credit1:  When obtaining each new input, the prompt message will begin with a number
;			representing the count of valid integers recieved so far.


INCLUDE Irvine32.inc


; Macro definitions
; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Prompts user to enter a string
; Reads and saves string input form user.
; Saves length of inputed string to memory at provided location
;
; Preconditions: do not use edx, eax, ecx, edi as arguments
;
; Receives:
;   promptOffset = prompt address
;   inputOffset = Address to put user input at
;   maxStringLength = Maximum allowable length for user's inputed string
;   userStringLength = Address to save actual length of users string
;
; Returns: 
;	inputOffset = generated string address
;   userStringLength = length of string saved here
; ---------------------------------------------------------------------------------
mGetString MACRO promptOffset, inputOffset, maxStringLength, userStringLength
  PUSH		EDX
  PUSH		EAX
  PUSH		ECX
  PUSH		EDI

  ; Print user prompt
  MOV		EDX, promptOffset
  CALL		WriteString

  ; Get user input
  MOV		EDX, inputOffset
  MOV		ECX, maxStringLength
  CALL		ReadString

  ; Save string length
  MOV		EDI, userStringLength
  MOV		[EDI], EAX

  POP		EDI
  POP		ECX
  POP		EAX
  POP		EDX
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Prints string saved at the given address
;
; Preconditions: do not use edx as argument
;
; Receives:
;   stringOffset = address of string to print
;
; Returns: none
; ---------------------------------------------------------------------------------
mDisplayString MACRO stringOffset
  PUSH		EDX
  MOV		EDX, stringOffset
  CALL		WriteString
  POP		EDX
ENDM

; Constant definitions
COUNT = 10			; Array length

.data
intro			BYTE	"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures ",13,10
				BYTE	"Written by: Kaewan Gardi ",13,10,0
description		BYTE	"Please provide 10 signed decimal integers.",13,10
				BYTE	"Each number needs to be small enough to fit inside a 32 bit register. ",13,10
				BYTE	"After you have finished inputting the raw numbers I will display a list",13,10
				BYTE	"of the integers, their sum, and their average value.",13,10,0
farewellMsg		BYTE	"Thanks for playing!",13,10,0
inputPrompt		BYTE	": Please enter a signed number: ",0
tryAgainMsg		BYTE	": Please try again: ",0
badInput		BYTE	"ERROR: You did not enter a signed number or your number was too big. ",13,10,0
arrayMsg		BYTE	"You entered the following numbers: ",13,10,0
separator		BYTE	", ",0
sumMsg			BYTE	"The sum of these numbers is: ",0
avgMsg			BYTE	"The rounded average is: ",0
inputArray		SDWORD	COUNT DUP(?)
inputLength		DWORD	?					; Number of characters in user input
stringBuffer	BYTE	20 DUP(0)			; Input buffer for user input
inputDigits		BYTE	20 DUP(?)			; User's input saved here after ASCII values are adjusted down
revString		BYTE	20 DUP(0)			; Int is converted to a string in reverse order into this string
userInt			SDWORD	?
sum				SDWORD	?
average			SDWORD	?
validInputs		DWORD	0
extraCred		BYTE	"**EC1: Number each line of user input and display a running subtotal", 13,10
				BYTE	"of the user's valid numbers. These displays must use WriteVal. (1 pt)",13,10,0

.code
main PROC
  ; Print introduction
  PUSH			OFFSET extraCred
  PUSH			OFFSET description
  PUSH			OFFSET intro
  CALL			introduction

  ; Get 10 inputs from users. Save to array
  PUSH			OFFSET revString
  PUSH			OFFSET validInputs
  PUSH			OFFSET inputArray
  PUSH			OFFSET userInt
  PUSH			OFFSET tryAgainMsg
  PUSH			OFFSET badInput
  PUSH			OFFSET inputDigits
  PUSH			SIZEOF stringBuffer
  PUSH			OFFSET inputLength
  PUSH			OFFSET stringBuffer
  PUSH			OFFSET inputPrompt
  CALL			getInputs

  ; Display array back to user
  PUSH			OFFSET separator
  PUSH			OFFSET arrayMsg
  PUSH			OFFSET revString
  PUSH			OFFSET stringBuffer
  PUSH			OFFSET inputArray
  CALL			displayArray

  ; Calculate and show sum
  PUSH			OFFSET revString
  PUSH			OFFSET stringBuffer
  PUSH			OFFSET sum
  PUSH			OFFSET sumMsg
  PUSH			OFFSET inputArray
  CALL			showSum


  ; Calculate and show average
  PUSH			OFFSET avgMsg
  PUSH			OFFSET average
  PUSH			OFFSET revString
  PUSH			OFFSET stringBuffer
  PUSH			sum
  CALL			showAverage

  ; Print farewell
  PUSH			OFFSET farewellMsg
  call			farewell


	Invoke ExitProcess,0	; exit to operating system
main ENDP


;---------------------------------------------------------------------------------
; Name: introduction
;
; Print the Introduction message with the progrom and author name, followed by a
;		description of the Extra credit implemented.
;		Then print a brief description of the program with instructions.
;
; Receives: 
;		[EBP + 8]		= OFFSET intro (reference, input)
;		[EBP + 12]		= OFFSET instructions (reference, input)
;		[EBP + 16]		= OFFSET extraCred (reference, input)
;---------------------------------------------------------------------------------
introduction PROC
  PUSH			EBP
  MOV			EBP, ESP

  ; Print title and name
  mDisplayString [EBP + 8]
  CALL			Crlf

  mDisplayString [EBP + 16]
  CALL			Crlf

  ; Print description
  mDisplayString [EBP + 12]
  CALL			Crlf

  POP			EBP
  RET			12
introduction ENDP


;---------------------------------------------------------------------------------
; Name: ReadVal
;
; Prints a number representing how many valid inputs have been recieved at the
;	beginning of each prompt.  Prompts the user to enter an integer and saves it
;	as a string.  The string characters then have 48 subtracted from them to get
;	the interger values.  Then the input is validated.
;	If the first character is not 0-9, or a '+' or '-', an error is displayed and
;	we loop back and ask for a new input.
;	If the remaining characters are not 0-9, then the same error/looping executes.
;	The string is then converted to an SDWORD and saved in memory.
;	A tens place accumulator tracks what 10s place we are at.  We move backwards through
;	the string, multiplying each digit by the appropriate 10s multiple and adding the
;	result to the growing value in memory.
;	If the initial value was negative, the same occurs except the 10s place multiple
;	will be negative, so the value in memory will grow negatively as values are added.
;
; Preconditions: Size allocated for stringBuffer, revString, inputDigits should all be
;		large enough to fit the expected inputs
; Postconditions:  stringBuffer, inputDigits, revString will contain some leftover
;		values from the calculations and conversions
;
; Receives: 
;		[EBP + 8]		= OFFSET inputPrompt(reference, input)
;		[EBP + 12]		= OFFSET stringBuffer(reference, input)
;		[EBP + 16]		= OFFSET inputLength(reference, input/output)
;		[EBP + 20]		= SIZEOF stringBuffer(value, input/output)
;		[EBP + 24]		= OFFSET inputDigits(reference, output)
;		[EBP + 28]		= OFFSET badInput(reference, input/output)
;		[EBP + 32]		= OFFSET tryAgainMsg(reference, input)
;		[EBP + 36]		= OFFSET userInt(reference, output)
;		[EBP + 40]		= OFFSET validInputs(reference, input)
;		[EBP + 44]		= OFFSET revString(reference, input/output)
; Returns: userInt will store the input signed integer as an SDWORD
;---------------------------------------------------------------------------------
ReadVal PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

  ; Print running valid integer total
  PUSH			[EBP + 44]
  PUSH			[EBP + 12]
  PUSH			[EBP + 40]
  CALL			WriteVal

  ; Print prompt and get user input
  mGetString	[EBP + 8], [EBP + 12], [EBP + 20], [EBP + 16]


  ; Get BYTE ASCII value and subtract 48
_convertASCII:
  MOV			ESI, [EBP + 12]		; Raw user string
  MOV			EDI, [EBP + 24]		; Future digit string
  MOV			EBX, [EBP + 16]		; Length of input
  MOV			ECX, [EBX]

  ; Validate input was length > 0
  CMP			ECX, 0
  JLE			_badInput
  CLD

_convertASCIILoop:
  ; Subtract 48 from every digit
  LODSB
  SUB			AL, 48
  STOSB
  LOOP			_convertASCIILoop

  ; Validate input characters
  MOV			ESI, [EBP + 24]
  MOV			EBX, [EBP + 16]
  MOV			ECX, [EBX]		; Number of digits to ECX
  CLD
  LODSB

;--------------------------------------------------------------------------------
;Validate user input.  If invalid, print error and get new input.
;		Detailed summary on validation is above in procedure description
;
;--------------------------------------------------------------------------------
_validateFirstDigit:
  CMP			AL, 9			; Is char > 9
  JG			_badInput
  CMP			AL, 0			; Is char > 0
  JGE			_preValidateNext
  CMP			AL, -3			; Is char a - sign
  JE			_signPreValidateNext
  CMP			AL, -5			; Is char a + sign
  JE			_signPreValidateNext
  JMP			_badInput

_signPreValidateNext:
  ; Check if all we got was a + or - sign with no more digits
  DEC			ECX
  CMP			ECX, 0			
  JE			_badInput	
  JMP			_validateNext
  
_preValidateNext:
  ; Check if no more digits 
  DEC			ECX
  CMP			ECX, 0			
  JE			_convertSDWORD	

_validateNext:
  LODSB
  CMP			AL, 9			; Is char < 9
  JG			_badInput
  CMP			AL, 0			; Is char > 0
  JL			_badInput
  LOOP			_validateNext
  JMP			_convertSDWORD	


  ; Print error and get new input 
_badInput:
  ; Print error
  mDisplayString [EBP + 28]

  ; Print running valid integer total
  PUSH			[EBP + 44]
  PUSH			[EBP + 12]
  PUSH			[EBP + 40]
  CALL			WriteVal

  ; Get new input
  mGetString	[EBP + 32], [EBP + 12], [EBP + 20], [EBP + 16]
  JMP			_convertASCII

;--------------------------------------------------------------------------------
;  Convert string of digits to SDWORD.  Positive conversions are treated first.
;		If input was negative, then we jump down to a negative int conversion blok
;		More details in procedure description
;
;--------------------------------------------------------------------------------
_convertSDWORD:
  ; Clear destination memory
  MOV			EDI, [EBP + 36]
  MOV			EBX, 0
  MOV			[EDI], EBX
  ; is positive or negative number?
  MOV			ESI, [EBP + 24]
  LODSB
  CMP			AL, -3
  JE			_convertNegativeSDWORD

  ; Get index of last digit to ESI
  MOV			ESI, [EBP + 24]
  MOV			EAX, [EBP + 16]
  ADD			ESI, [EAX]
  DEC			ESI
  STD

  ; Move 1's digit to [EDI] (userInt)
  MOV			EDI, [EBP + 36]			; Final int address to EDI
  LODSB
  MOV			[EDI], AL

  ; Are we done? 
  CMP			ESI, [EBP + 24]
  JL			_goodInput

  MOV			EBX, 1					; EBX will hold appropriate multiple of 10

  ; Add next 10's place digit to userInt
_addNextDigit:
  ; Set EBX to next multiple of 10
  MOV			EAX, 10
  MUL			EBX
  MOV			EBX, EAX			
  
  ; Get next 10s value
  MOV			EAX, 0					; Clear EAX
  LODSB
  CMP			AL, -5					; Is this a real digit or a + sign?
  JE			_goodInput
  MUL			EBX						; Multiply digit by current 10s multiple

  ; Add to userInt
  ADD			[EDI], EAX
  JO			_badInput				; Is number too big?

  ; Are we done? 
  CMP			ESI, [EBP + 24]
  JL			_goodInput
  JMP			_addNextDigit

;--------------------------------------------------------------------------------
;  Convert string of digits to negative SDWORD.  Input began with a '-' so we
;		here.  Code is almost identical to above, except values are negatively
;		added to the accumulated total.
;
;--------------------------------------------------------------------------------
_convertNegativeSDWORD:
  ; Get index of last digit
  MOV			ESI, [EBP + 24]
  MOV			EAX, [EBP + 16]
  ADD			ESI, [EAX]
  DEC			ESI
  STD

  ; Move 1's digit to [EDI] (userInt)
  MOV			EDI, [EBP + 36]
  MOV			EAX, 0					; Clear EAX
  LODSB
  MOV			EBX, 0
  MOV			[EDI], EBX
  SUB			[EDI], EAX

  ; Are we done? 
  CMP			ESI, [EBP + 24]
  JL			_goodInput

  MOV			EBX, -1					; EBX will hold appropriate multiple of 10

  ; Add next 10's place digit to userInt
_addNextNegDigit:
  ; Set EBX to next multiple of 10
  MOV			EAX, 10
  IMUL			EBX
  MOV			EBX, EAX			
  
  ; Get next 10s value
  MOV			EAX, 0					; Clear EAX
  LODSB
  CMP			AL, -3					; Is this a real digit or a sign?
  JE			_goodInput
  IMUL			EBX						; Multiply digit by current 10s multiple

  ; Add to userInt
  ADD			[EDI], EAX
  JO			_badInput				; Is number too big/small?

  ; Are we done? 
  CMP			ESI, [EBP + 24]
  JL			_goodInput
  JMP			_addNextNegDigit

_goodInput:
  POPAD
  POP			EBP
  RET			40
ReadVal ENDP


;---------------------------------------------------------------------------------
; Name: WriteVal
;
; Converts the SDWORD at [EBP + 8] to a String and prints it to the screen.
;	The SDWORD is repeatedly divided by 10 with the remainder saved to [EBP + 16].
;	A '-' will be appended if needed.
;	[EBP + 16] is then reverse and saved at [EBP + 12] to hold the final
;	converted string.
;	The string is then printed
;
; Preconditions: stringBuffer, revString are of size 20 Bytes
;
; Postconditions: [EBP + 12] Will hold the converted string offset
;			[EBP + 16] will hold the converted string in reverse
;
; Receives: 
;		[EBP + 8]		= OFFSET of integer(reference, input)
;		[EBP + 12]		= OFFSET stringBuffer(reference, output)
;		[EBP + 16]		= OFFSET revString(reference, output)
; Returns: None
;---------------------------------------------------------------------------------
WriteVal PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

;-----------------------------------------------------------------------
;  Initialize both strings that will be used to be filled with 0s
;
;-----------------------------------------------------------------------
  MOV			EDI, [EBP + 12]
  MOV			AL, 0
  MOV			ECX, 20
  CLD
  REP			STOSB					; Initialize string data with 0's

  ; Clear other String
  MOV			EDI, [EBP + 16]
  MOV			AL, 0
  MOV			ECX, 20
  CLD
  REP			STOSB					; Initialize string data with 0's

;-----------------------------------------------------------------------
; Convert SDWORD to string of ascii digits
;	Check if int is positive or negative and remember this.
;	The value in userInt is divided by 10 continuously until the dividen
;	is 0.  Each time the remainder is appended to revString with the added
;	ASCII offset (48).
;	This will build out final string in reverse.
;	If the int was negative, append a '-' to revString
;
;-----------------------------------------------------------------------
  MOV			ESI, [EBP + 8]

  ; Check if int is positive, negative, or 0:
  MOV			EDX, [ESI]
  CMP			EDX, 0
  JE			_zero
  JG			_convertPositiveInt

  ; Int is negative. Get absolute value of integer to help streamline conversion a bit
  MOV			EBX, 0
  MOV			EDX, [ESI]
  SUB			EBX, EDX		
  PUSH			1						; for later reference. 1 means int is negative
  PUSH			EBX						; Positive integer now at top of stack
  JMP			_convertNegInt

_convertPositiveInt:
  ; Build sting of digits in reverse.  We will reverse is to proper order at the end
  PUSH			0						; for later reference. 0 means int is positive
  PUSH			EDX						; Positive integer to top of stack
  MOV			EDI, [EBP + 16]			
  CLD
  MOV			ECX, 0					; Track how many BYTES final string will be
  JMP			_convertLoop

_convertNegInt:
  MOV			EDI, [EBP + 16]			; Point EDI back to our temperary reverse string buffer
  CLD
  MOV			ECX, 0					; Track how many BYTES final string will be.  

_convertLoop:
  ; Divide int by 10 and Load string in reverse
  POP			EAX
  MOV			EDX, 0
  MOV			EBX, 10
  DIV			EBX

  ; Get proper ASCII value for dividend
  ADD			EDX, 48
  PUSH			EAX						; Remaining integer value pushed to top of stack
  MOV			EAX, EDX
  STOSB									; Append digit's ASCII to string
  INC			ECX

  ; Are we done?
  MOV			EAX, [ESP]
  CMP			EAX, 0
  JNE			_convertLoop
  POP			EAX						; Realign the stack

  ; Was the value original int negative? add a '-' if so
  POP			EAX
  CMP			EAX, 0
  JE			_reverseString

  ; It was negative, add sign
  MOV			AL, 45
  CLD
  STOSB									
  INC			ECX
  JMP			_reverseString

;-----------------------------------------------------------------------
; The int is simply 0, so put the ASCII for 0 into the string and 
;	go to print it
;-----------------------------------------------------------------------
_zero:
  ; Move ASCII code for 0 into string
  MOV			EDI, [EBP + 12]
  MOV			AL, 48
  CLD
  STOSB
  JMP			_printString

;-----------------------------------------------------------------------
; Reverses the string in revString and places it into stringBuffer.
;	This is out final converted string. So we print it
;
;-----------------------------------------------------------------------
_reverseString:
  MOV			ESI, [EBP + 16]
  ADD			ESI, ECX		
  DEC			ESI						; Point to end of source string
  MOV			EDI, [EBP + 12]

  ; Reverse the string to proper order
_revLoop:
  STD
  LODSB
  CLD
  STOSB
  LOOP			_revLoop

_printString:
  mDisplayString [EBP + 12]

  POPAD
  POP			EBP
  RET			12
WriteVal ENDP


;---------------------------------------------------------------------------------
; Name: getInputs
;
; Loops through ReadVall 10 times to obtain new integers.  The integer returned to
;		userInt is appended to the output array after each ReadVal call.
;		Tracks the number of valid inputs given and passes updated value to
;		each ReadVal call (for EC1).
;
; Preconditions: Array to save inputs must be large enough to fit 10 SDWORDs
;
; Postconditions: revString, stringBuffer, inputLength, inputDigits, userInt
;			will be altered and hold some leftover values from the user
;			input and conversions.
;
; Receives: 
;		[EBP + 8]		= OFFSET inputPrompt(reference, input)
;		[EBP + 12]		= OFFSET stringBuffer(reference, output)
;		[EBP + 16]		= OFFSET inputLength(reference, output)
;		[EBP + 20]		= SIZEOF stringBuffer(value, input)
;		[EBP + 24]		= OFFSET inputDigits(reference, input)
;		[EBP + 28]		= OFFSET badInput(reference, input)
;		[EBP + 32]		= OFFSET tryAgainMsg(reference, input)
;		[EBP + 36]		= OFFSET userInt(reference, input)
;		[EBP + 40]		= OFFSET inputArray(reference, output)
;		[EBP + 44]		= OFFSET validInputs(reference, input/output)
;		[EBP + 48]		= OFFSET revString(reference, input/output)
; Returns: [EBP + 40] offset of the array will hold 10 user integer inputs (SDWORD)
;---------------------------------------------------------------------------------
getInputs PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

  MOV			EDI, [EBP + 40]			; set input Array destination 

  ; Call ReadVal 10 times to obtain new valid integers
  MOV			ECX, 10
_inputLoop:
  PUSH			[EBP + 48]
  PUSH			[EBP + 44]
  PUSH			[EBP + 36]	
  PUSH			[EBP + 32]		
  PUSH			[EBP + 28]	
  PUSH			[EBP + 24]
  PUSH			[EBP + 20]
  PUSH			[EBP + 16]
  PUSH			[EBP + 12]
  PUSH			[EBP + 8]
  CALL			ReadVal

  ; Save integer to array
  MOV			ESI, [EBP + 36]
  MOV			EAX, [ESI]
  MOV			[EDI], EAX
  ADD			EDI, 4

  ; Increment counter for number of valid integers
  MOV			EBX, [EBP + 44]
  MOV			EDX, 1
  ADD			[EBX], EDX
  LOOP			_inputLoop

  CALL			Crlf
  POPAD
  POP			EBP
  RET			44
getInputs ENDP


;---------------------------------------------------------------------------------
; Name: displayArray
;
; Prints a message titling the array.  Then prints the elements of the array
;		separated by a ", " sting
;
; Preconditions: Array must be filled with 10 SDWORDs
;
; Postconditions: revString, and stringBuffer will be altered to hold some leftover
;		data from calculations.  stringBuffer will contain the last printed integer
;		as a string
;
; Receives: 
;		[EBP + 8]		= OFFSET inputArray(reference, input)
;		[EBP + 12]		= OFFSET stringBuffer(reference, output)
;		[EBP + 16]		= OFFSET revString(reference, output)
;		[EBP + 20]		= OFFSET arrayMsg(reference, output)		
;		[EBP + 24]		= OFFSET separator(reference, input)
; Returns: None
;---------------------------------------------------------------------------------
displayArray PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

  ; Print message label
  mDisplayString [EBP + 20]

  ; Call Writeval 10 times to display all elements of the array
  MOV			ECX, 10
  MOV			ESI, [EBP + 8]
_printLoop:
  PUSH			[EBP + 16]
  PUSH			[EBP + 12]
  PUSH			ESI							;OFFSET of current array element
  CALL			WriteVal

  ; Do we need a comma separator?
  CMP			ECX, 1
  JG			_addComma

_loopConditional:
  ADD			ESI, 4						; Point to next element
  LOOP			_printLoop
  JMP			_return

_addComma:
  mDisplayString [EBP + 24]
  JMP			_loopConditional

_return:
  CALL			Crlf
  POPAD
  POP			EBP
  RET			20
displayArray ENDP


;---------------------------------------------------------------------------------
; Name: showSum
;
; Print sum title message. Calculates the sum of all elements in the array.  
;		Save sum to memory and call WriteVal to display the sum.
;
; Preconditions: inputArray must be filled with 10 SDWORDs
; Postconditions: stringBuffer, and revStringwill be altered by the called WriteVal
;		procedure.  stringBuffer will contain the string representation of the sum.
; Receives: 
;		[EBP + 8]		= OFFSET inputArray(reference, input)
;		[EBP + 12]		= OFFSET sumMsg(reference, input)
;		[EBP + 16]		= OFFSET sum(reference, output)
;		[EBP + 20]		= OFFSET stringBuffer(reference, input)
;		[EBP + 24]		= OFFSET revString(reference, input)
; Returns: [EBP + 16] (sum) will hold the sum of all elements of the array
;---------------------------------------------------------------------------------
showSum PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

  ; Display title message
  mDisplayString [EBP + 12]


  ; Calculate sum
  MOV			ESI, [EBP + 8]
  MOV			EAX, 0
  MOV			ECX, 10
_addElement:
  ADD			EAX, [ESI]
  ADD			ESI, 4
  LOOP			_addElement

  ; Move sum to memory
  MOV			EDI, [EBP + 16]
  MOV			[EDI], EAX


  ; CALL WriteVall
  PUSH			[EBP + 24]
  PUSH			[EBP + 20]
  PUSH			EDI							;OFFSET of current array element
  CALL			WriteVal
  CALL			Crlf

  POPAD
  POP			EBP
  RET			12
showSum ENDP


;---------------------------------------------------------------------------------
; Name: showAverage
;
; Display average title message. Calculate average by dividing sum by 10.
;		The average is rounded down to the nearest integer.
;		Average is then saved to memory.
;		WriteVal is called to display the average.
;
; Preconditions: Assumes sum was calculated from an array of 10 integers.
;
; Postconditions: stringBuffer, and revString will be altered by the called WriteVal
;		procedure.  stringBuffer will contain the string representation of the sum.
;
; Receives: 
;		[EBP + 8]		= sum value(value, input)
;		[EBP + 12]		= OFFSET stringBuffer(reference, input)
;		[EBP + 16]		= OFFSET revString(reference, input)
;		[EBP + 20]		= OFFSET average(reference, output)		
;		[EBP + 24]		= OFFSET avgMessage(reference, input)
; Returns: None
;---------------------------------------------------------------------------------
showAverage PROC
  PUSH			EBP
  MOV			EBP, ESP
  PUSHAD

  ; Print message
  mDisplayString [EBP + 24]

  ; Calculate Average
  MOV			EAX, [EBP + 8]
  CDQ
  MOV			EBX, 10
  IDIV			EBX				; sum / 10
  
  ; Save average to memory variable
  MOV			EDI, [EBP + 20]
  MOV			[EDI], EAX		

  ; Display average
  PUSH			[EBP + 16]
  PUSH			[EBP + 12]
  PUSH			[EBP + 20]
  CALL			WriteVal

  CALL			Crlf
  CALL			Crlf
  POPAD
  POP			EBP
  RET			20
showAverage ENDP


;---------------------------------------------------------------------------------
; Name: farewell
;
; Print a farewell mesasge
;
; Receives: 
;		[EBP + 8]		= farewell message offset (reference, input)
;---------------------------------------------------------------------------------
farewell PROC
  PUSH			EBP
  MOV			EBP, ESP

  ; Print farewell message
  mDisplayString [EBP + 8]

  POP			EBP
  RET			4
farewell ENDP


END main
