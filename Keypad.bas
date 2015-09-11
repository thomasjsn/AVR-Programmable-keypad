'--------------------------------------------------------------
'                   Thomas Jensen | uCtrl.net
'--------------------------------------------------------------
'  file: KEYPAD_v.1.0
'  date: 17/11/2007
'--------------------------------------------------------------
$regfile = "attiny2313.dat"
$crystal = 8000000
Config Portd = Input
Config Portb = Output
Config Watchdog = 1024

Dim Lifesignal As Byte , Number As Byte , Row As Byte , Button As Byte
Dim Code1 As Integer , Code2 As Integer , Code3 As Byte , Code4 As Byte , Nr As Byte
Dim Redled As Byte , Greenled As Byte , Amberled As Byte , Code As Integer
Dim Code_p As Integer , Kanal1 As Byte , Kanal2 As Byte , Timeout As Byte , Feil As Integer
Dim Eeprom As Eram Integer , Program As Byte , Feil_a As Byte

'Inn
'PD2 Tastatur 1
'PD3 Tastatur 2
'PD4 Tastatur 3
'PD5 Tastatur 4

'Ut
'PB0 Tastatur 1
'PB1 Tastatur 2
'PB2 Tastatur 3
'PB3 Kanal 1
'PB4 Kanal 2
'PB5 Grønn LED
'PB6 Rød LED
'PB7 Lifesignal(til Msmu2)

Code_p = Eeprom                                             'Get stored code
If Code_p < 0 Then Code_p = 1234                            'If no code use default
Number = 255

'Boot
Portb = 0

Start Watchdog

Main:
'Read keypad
If Button = 0 And Feil = 0 And Number = 255 Then

If Row = 0 Then                                             'Keyboard row 1
   Portb.0 = 1
   If Pind.5 = 1 Then Number = 10
   If Pind.4 = 1 Then Number = 7
   If Pind.3 = 1 Then Number = 4
   If Pind.2 = 1 Then Number = 1
   Portb.0 = 0
   End If

If Row = 1 Then                                             'Keyboard row 2
   Portb.1 = 1
   If Pind.5 = 1 Then Number = 0
   If Pind.4 = 1 Then Number = 8
   If Pind.3 = 1 Then Number = 5
   If Pind.2 = 1 Then Number = 2
   Portb.1 = 0
   End If

If Row = 2 Then                                             'Keyboard row 3
   Portb.2 = 1
   If Pind.5 = 1 Then Number = 11
   If Pind.4 = 1 Then Number = 9
   If Pind.3 = 1 Then Number = 6
   If Pind.2 = 1 Then Number = 3
   Portb.2 = 0
   End If

Row = Row + 1                                               'Next row
If Row > 2 Then Row = 0
End If

'Reset button status
Portb.0 = 1
Portb.1 = 1
Portb.2 = 1
If Pind.2 = 0 And Pind.3 = 0 And Pind.4 = 0 And Pind.5 = 0 Then Button = 0
Portb.0 = 0
Portb.1 = 0
Portb.2 = 0

'Get code
If Number <> 255 Then
Button = 1
Select Case Nr
   Case 0                                                   'Digit 1
   Code1 = Number
   Code1 = Code1 * 1000
   Code = Code1 + Code
   Amberled = 4
   Case 1                                                   'Digit 2
   Code2 = Number
   Code2 = Code2 * 100
   Code = Code2 + Code
   Amberled = 4
   Case 2                                                   'Digit 3
   Code3 = Number
   Code3 = Code3 * 10
   Code = Code3 + Code
   Amberled = 4
   Case 3                                                   'Digit 4
   Code4 = Number
   Code = Code4 + Code
   Amberled = 4

   Case 4
   If Program = 1 Then                                      'Program new code
      Select Case Number
         Case 11                                            'Accept
         Code_p = Code
         Eeprom = Code_p
         Greenled = 60
         Case Else                                          'Reject
         Redled = 30
         End Select
      Number = 255
      Program = 0
      Gosub Reset_code_read
      Goto Main
      End If
   If Code = Code_p Then                                    'Code correct
   Feil_a = 0
      Select Case Number
         Case 0                                             'New code
         Program = 1
         Amberled = 40
         Case 10                                            'Channel 1
         Kanal1 = 20
         Greenled = 20
         Case 11                                            'Channel 2
         Kanal2 = 20
         Greenled = 40
         Case Else                                          'Reject
         Redled = 30
         End Select
   Else                                                     'Code failed
      Redled = 30
      Feil_a = Feil_a + 1
   End If
   Timeout = 0
   Number = 255
   Gosub Reset_code_read
   Goto Main
End Select
Nr = Nr + 1
Number = 255
End If

'Timeout when entering code
If Nr > 0 And Timeout = 0 Then Timeout = 201
If Timeout > 0 Then Timeout = Timeout - 1
If Timeout = 1 And Nr <> 0 Then
   Gosub Reset_code_read
   Number = 255
   Redled = 30
   End If

'Error handling, 3 wrong codes
If Feil_a > 3 And Feil = 0 Then
   Feil = 1200
   Feil_a = 0
   End If
If Feil > 0 And Redled = 0 Then Redled = 4
If Feil > 0 Then Feil = Feil - 1

'AmberLED
If Amberled > 0 Then
   Greenled = Amberled
   Redled = Amberled
   Amberled = 0
   End If

'GreenLED
If Greenled > 2 Then Portb.5 = 1
If Greenled = 2 Then Portb.5 = 0
If Greenled > 0 Then Greenled = Greenled - 1

'RedLED
If Redled > 2 Then Portb.6 = 1
If Redled = 2 Then Portb.6 = 0
If Redled > 0 Then Redled = Redled - 1

'Channel1
If Kanal1 = 20 Then Portb.3 = 1
If Kanal1 = 1 Then Portb.3 = 0
If Kanal1 > 0 Then Kanal1 = Kanal1 - 1

'Channel2
If Kanal2 = 20 Then Portb.4 = 1
If Kanal2 = 1 Then Portb.4 = 0
If Kanal2 > 0 Then Kanal2 = Kanal2 - 1

'Lifesignal
If Lifesignal > 0 Then Lifesignal = Lifesignal - 1
If Lifesignal = 12 Then Portb.7 = 1
If Lifesignal = 1 Then Portb.7 = 0
If Lifesignal = 0 Then Lifesignal = 41

Reset Watchdog
Waitms 50
Goto Main                                                   'Loop cycle
End

Reset_code_read:                                            'Reset code param.
   Nr = 0
   Code = 0
   Code1 = 0
   Code2 = 0
   Code3 = 0
   Code4 = 0
Return
End