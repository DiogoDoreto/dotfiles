# AutoHotKey

## Schedule AutoHotKey script with Task Scheduler

You can use the Task Scheduler if you want more control over how a program starts with Windows. For example, this method is useful to delay start a program. Let me show you how to schedule and run an AutoHotKey script on startup in Windows.

    Press the "Start" key.
    Search and open "Task Scheduler."
    Click "Create Basic Task."
    Type a name in the "Name" field.
    Click "Next."
    Select "When I log on."
    Press "Next."
    Choose "Start a program."
    Click "Next."
    Type the below path in the "Program/Script" field.
    "C:\Program Files\AutoHotkey\AutoHotkey.exe"
    Type the AHK script file path in the "Arguments" field.
    Press the "Finish" button.
