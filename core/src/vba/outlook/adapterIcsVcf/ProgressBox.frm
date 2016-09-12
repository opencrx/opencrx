VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} ProgressBox 
   Caption         =   "UserForm1"
   ClientHeight    =   3120
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4710
   OleObjectBlob   =   "ProgressBox.frx":0000
   ShowModal       =   0   'False
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "ProgressBox"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' Implements a progress box with a progress bar and space for user-defined text above the bar
' Uses Microsoft's Forms library (by default available with all office/VBA installations)
' To use in your VBA project:
' 1) Make sure that the "Microsoft Forms" object library is checked in Tools/References
' 2) Insert a blank User Form
' 3) Rename the user form to "ProgressBox"
' 4) Set the user form property "showModal" to false (so you can do other things while the dialog is displayed)
' 5) Show the code for the User Form, and highlight/delete everything
' 6) Insert this file (using insert/file) into the code for the User Form
' 7) Add appropriate code to your VBA routine where you want to show progress:
'     * ProgressBox.Show --- shows the progress box. Include this before starting processing.
'     * ProgressBox.Increment newPercent (single), NewText (optional string) --- updates the progress bar and optionally changes the text
'     * ProgressBox.Hide --- removes the progress bar. Include this at the end of processing.
' 8) Optionally, you can get/set the percentage and the text individually using the "Percent" and "Text" properties, followed by calling ProgressBox.repaint

Private Const DefaultTitle = "Progress"
Private myText As String
Private myPercent As Single

' Text property shows user-defined text above the progress bar
Public Property Let Text(newText As String)
  If newText <> myText Then
    myText = newText
    Me.Controls("UserText").Caption = myText
    Call sizeToFit
  End If
End Property

Public Property Get Text() As String
  Text = myText
End Property

' Percent property alters the progress bar
Public Property Let Percent(newPercent As Single)
  If newPercent <> myPercent Then
    ' limit percent to between 0 and 100
    myPercent = Min(max(newPercent, 0#), 100#)
    Call updateProgress
  End If
End Property

Public Property Get Percent() As Single
  Percent = myPercent
End Property

' Increment method enables the percent and optionally the text to be updated at same time
Public Sub Increment(ByVal newPercent As Single, Optional ByVal newText As String)
  Me.Percent = newPercent
  If newText <> "" Then Me.Text = newText
  Call updateTitle
  Me.Repaint
End Sub

' Setup the progress dialog - title, control layout/size etc.
Private Sub UserForm_Initialize()
  Call setupControls
  Call updateTitle
End Sub

' Prevents use of the Close button
Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
  If CloseMode = vbFormControlMenu Then Cancel = True
End Sub

' Removes any current controls, add the needed controls ...
Private Sub setupControls()
  Dim i As Integer
  Dim aControl As Label
  ' remove existing controls
  For i = Me.Controls.count To 1 Step -1
    Me.Controls(i).Remove
  Next i
  ' add user text - don't worry about positioning as "sizeToFit" takes care of this
  Set aControl = Me.Controls.Add("Forms.Label.1", "UserText", True)
  aControl.Caption = ""
  aControl.AutoSize = True
  aControl.WordWrap = True
  aControl.Font.Size = 8
  ' add progressFrame - don't worry about positioning as "sizeToFit" takes care of this
  Set aControl = Me.Controls.Add("Forms.Label.1", "ProgressFrame", True)
  aControl.Caption = ""
  aControl.Height = 16
  aControl.SpecialEffect = fmSpecialEffectSunken
  ' add user text - don't worry about positioning as "sizeToFit" takes care of this
  Set aControl = Me.Controls.Add("Forms.Label.1", "ProgressBar", True)
  aControl.Caption = ""
  aControl.Height = 14
  aControl.BackStyle = fmBackStyleOpaque
  aControl.BackColor = &HFF0000 ' Blue
  ' position the controls and size the progressBox
  Call sizeToFit
End Sub


' Adjusts positioning of controls/size of form depending on size of user text
Private Sub sizeToFit()
  ' setup width of progress box
  Me.Width = 240
  ' user-supplied text should be topmost, taking up the appropriate size ...
  Me.Controls("UserText").Top = 6
  Me.Controls("UserText").Left = 6
  Me.Controls("UserText").AutoSize = False
  Me.Controls("UserText").Font.Size = 8
  Me.Controls("UserText").Width = Me.InsideWidth - 12
  Me.Controls("UserText").AutoSize = True
  ' progress frame/bar should be below user text
  Me.Controls("ProgressFrame").Top = Int(Me.Controls("UserText").Top + Me.Controls("UserText").Height) + 6
  Me.Controls("ProgressFrame").Left = 6
  Me.Controls("ProgressFrame").Width = Me.InsideWidth - 12
  Me.Controls("ProgressBar").Top = Me.Controls("ProgressFrame").Top + 1
  Me.Controls("ProgressBar").Left = Me.Controls("ProgressFrame").Left + 1
  Call updateProgress ' update ProgressBar width
  ' finally, height of progress box should fit around text and progress bar & allow for title/box frame
  Me.Height = Me.Controls("ProgressFrame").Top + Me.Controls("ProgressFrame").Height + 6 + (Me.Height - Me.InsideHeight)
End Sub

' updates the caption of the progress box to keep track of progress
Private Sub updateTitle()
  If (Int(myPercent) Mod 5) = 0 Then
    Me.Caption = DefaultTitle & " - " & Format(Int(myPercent), "0") & "% Complete"
  End If
End Sub

' updates the width of the progress bar to match the current percentage
Private Sub updateProgress()
  If myPercent = 0 Then
    Me.Controls("ProgressBar").Visible = False
  Else
    Me.Controls("ProgressBar").Visible = True
    Me.Controls("ProgressBar").Width = Int((Me.Controls("ProgressFrame").Width - 2) * myPercent / 100)
  End If
End Sub

' Min and Max functions
Private Function Min(number1 As Single, number2 As Single) As Single
  If number1 < number2 Then
    Min = number1
  Else
    Min = number2
  End If
End Function

Private Function max(number1 As Single, number2 As Single) As Single
  If number1 > number2 Then
    max = number1
  Else
    max = number2
  End If
End Function
