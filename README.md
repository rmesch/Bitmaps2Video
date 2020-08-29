# Bitmaps2Video
A Delphi-class to support encoding of a series of bitmaps to a video file. It requires the ffmpeg-library and is intended as an easy to use interface to this library.

The cross platform demo requires at least Delphi 10.3.3 as it contains the necessary support for requesting permission to write to external storage at runtime.
The video created on Android is stored in public downloads folder and the Androi demo compiles for both 32 and 64 bit.

Shared objects for iOS are supplied, but none of the current developer has a working iOS development setup so they have not been added to the deployment 
manager and it has thus also not been tested on iOS yet. Feel free to do so and create a pull request once you succeeded.
