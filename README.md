# DMS Windows MIDI Monitor

## Description
A MIDI Monitor utility for Windows that sends, receives, and displays MIDI short messages and System Exclusive messages from MIDI devices connected to the computer. I wrote this program because I needed a simple utility as a basis to send and analyze received MIDI messages to my Roland GX100.

It was written using the Lazaurs IDE and the Free Pascal compiler.

![Screen shot](/images/MidiMonitor.png)

## Features
- Full Lazarus Pascal source code is included.
- Receive and display short midi messages. Short messages received are shown in hex and english.
- Send midi or sysex messages to a midi device. 
- You can name and save the messages you have sent for later use.
- Automatically calculates a Roland checksum for sysex messages.
- Allows comments in the saved messages. Just start the line with a semicolon.

## Requirements
- Windows 7 or later.
- A MIDI IN and/or MIDI OUT device connected to your computer.
- Lazarus IDE and Free Pascal compiler if you want to compile the program yourself.

## Program Installation
All you need is the executable file. Nothing is installed on your computer and the registry is untouched. An ini file is created in the same directory as the executable to save your MIDI device settings and saved messages.
1. Download the dmsmidimon.zip file from the releases page. 
2. Run it. No installation is required.

## Source Code Installation
1. Clone the repository.
2. Open Lazarus and load the project file (dmsmidimon.lpr)
3. Compile and run.

## Usage
Note that hovering your mouse cursor over most of the gui controls will display a hint that explains what the control does.

### The [Settings] tab
![Screen shot](/images/Settings.png)
### MIDI In / Out
Select your MIDI IN and/or MIDI OUT devices. Then check the [Open] checkbox to open each device. The selected device names are saved to an ini file when you open them. They will be automatically selected and re-opened the next time you run the program.

*NOTE: If your device has a way to make the MIDI OUT port act as a MIDI THRU port, disable that function on your device. Otherwise, any midi you send will be echoed back to the MIDI IN device then out again and cause a feedback loop.*

### Millisecond to delay between sending sysex messages

Number of milliseconds to delay between sending sysex messages.
This allows devices a bit of time to digest sysex messages. It is useful when you enter multiple messages in "Midi to Send OUT". You can leave it set to zero unless necessary. Range is 0 to 1000 milliseconds. (1000 milliseconds = 1 second)

### The [Midi Monitor] tab

#### [Midi to Send OUT] - Sending MIDI messages
*NOTE: To send a MIDI message, the MIDI OUT device needs to be selected.*
Enter the MIDI bytes to send in Hex format. 

You can separate the bytes with a space or not. You can add comments to the message by starting the line with a semicolon ";".
```
; This is a comment because it starts with a semicolon
F0 41 10 00 00 00 00 0B 12 20 00 00 00 00 00 00 01 F7
```

If you want to include a checksum byte, enclose the bytes that you want calculated in brackets '[]'. The checksum will be inserted after the closing bracket. For example, if the midi message is
```
F0 41 10 00 00 00 00 0B 12 [20 00 00 00 00 00 00 01] F7 
the checksum will be calculated and inserted after the closing bracket: 
F0 41 10 00 00 00 00 0B 12 20 00 00 00 00 00 00 01 *5F* F7
```

Then click the [Send] button to send the message.

##### [Saved Messages] - Saving and naming your sent MIDI messages
The [Saved messages] section lets you save, load, or delete messages. To Save the current text in the [Midi to Send Out] editor, Type in a name to save as and click the [Save] button. To load a saved message, select the name from the combobox and click the [Load] button. To delete a saved message, select the name from the combobox and click the [Delete] button.

#### [Midi Messages IN] - Recieving Short MIDI messages
There are 2 windows that display the MIDI messages received from the MIDI IN device. You are free to select the hex bytes in these windows and copy them to the clipboard, edit, delete, or do anything you can do in a simple text editor.

#### [Sysex Messages IN] - Receiving sysex MIDI messages
System Exclusive messages received will automatically be displayed in the **[Midi SysEx Messages IN]** window as hex bytes.

##### Saving and naming your sent MIDI messages
The [Saved messages] combobox lets you save, load, or delete messages you have sent.
- To save a message, enter some text in the combobox and hit the [Save] button. The text in the [MIDI to Send OUT] is saved or updated. It is stored in dmsmidimon.msgdb.
- To load a message, select the text in the combobox and hit the [Load] button. The MIDI in the [MIDI to Send OUT] will be replaced with the saved message.
- To delete a message, select the text in the combobox and hit the [Delete] button. The saved message will be deleted from dmsmidimon.msgdb.

### The [Calc] tab
![Screen shot](/images/Calc.png)
This tab has a few conversion calculators that you might find useful. Enter your info in the Input box and the result will be displayed in the memo box. If you have any questions about how to use them, hover the mouse over one of the buttons in question to see a hint.

# License
This project is licensed under the MIT License - see the LICENSE file for details.

# Acknowledgments
- I used the Lazarus IDE and Free Pascal compiler to write the program.
- The midi. pas file has existed on the internet for years. I've made several modifications to it to bring it up to date with pascal features that didn't exist when it was first written.
- I found downloaded the program icon from icon-icons.com
