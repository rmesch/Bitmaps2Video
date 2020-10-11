# Bitmaps2Video
A Delphi-class to support encoding of a series of bitmaps to a video file. It requires the ffmpeg-library and is intended as an easy to use interface to this library. The class has been developed under Delphi 10.3.3, but should work with later versions and some previous versions, too. Please report issues.
We concentrate on writing video streams, there is only rudimentary support for adding audio streams.
The ffmpeg-library-version and header-file contained here orginate from
https://github.com/PassByYou888/FFMPEG-Header   Thanks!


There are 2 versions of the encoder class:

    TBitmapEncoder for platforms Win32 and Win64,
      relatively stable with H264, Mpeg-4, Mjpeg, Mpeg1/2 encoders and .avi, .mp4 containers.
 
    TBitmapEncoderM for cross platform with less features and less tested.
      Demo currently working with Win32, Win64, Android, Android64 thanks to TurboMagic

The cross platform demo requires at least Delphi 10.3.3 as it contains the necessary support for requesting permission to write to external storage at runtime.
The video created on Android is stored in public downloads folder and the Androi demo compiles for both 32 and 64 bit.

Shared objects for iOS are supplied, but none of the current developers has a working iOS development setup so they have not been added to the deployment manager and it has thus also not been tested on iOS yet. Feel free to do so and create a pull request once you succeeded.

Win-version: Methods no longer return booleans for success/failure, exceptions are thrown instead. Error handling needs to be improved. The method AddVideo now works with different frame rates, too. The added constructor CreateFromVideo still has some problems. Ideas, reports, fixes most welcome. Problems are described in more detail in the demo.

Cross-platform: Eventually the 2 versions of the class should be merged into one. For this more experience with platforms other than Windows is needed. If you can migrate any of the Win-routines to cross-platform, don't hesitate to do so :).

