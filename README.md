# Bitmaps2Video
A Delphi-class to support encoding of a series of bitmaps and video clips to a video file. It requires the ffmpeg-library and is intended as an easy to use interface to this library. The class has been developed under Delphi 10.3.3, but should work with later versions and some previous versions, too. Please report issues.
We concentrate on writing video streams, there is only rudimentary support for adding audio streams.
The ffmpeg-library-version and header-file contained here orginate from
https://github.com/PassByYou888/FFMPEG-Header   Thanks!


There are 2 versions of the encoder class:

    TBitmapEncoder VCL-based for platforms Win32 and Win64,
      relatively stable with H264, Mpeg-4, Mjpeg, Mpeg1/2 encoders and .avi, .mp4 containers.
 
    TBitmapEncoderM fmx-based for cross platform, a little less tested.
      Demo currently working with Win32, Win64, Android, Android64 thanks to TurboMagic.
      Recently more features translated from Win-version, more thoroughly tested under
      Android32, better demo.

The cross platform demo requires at least Delphi 10.3.3 as it contains the necessary support for requesting permission to write to external storage at runtime.
The video created on Android is stored in public downloads folder and the Android demo compiles for both 32 and 64 bit.

Shared objects for iOS are supplied, but none of the current developers has a working iOS development setup so they have not been added to the deployment manager and it has thus also not been tested on iOS yet. Feel free to do so and create a pull request once you succeeded.

Problem areas:

Error handling needs to be improved.

Win-version: 

The added constructor CreateFromVideo (Win version) has some problems.
 
Cross-platform version:

The H264-encoder does not work under Android (at least not Android32).
The use of TBitmap in threads has become more stable, but there might be issues remaining. Delphi claims thread-safety of TBitmap, but this does not seem to be the case, particularly under Android. Some workarounds are in place, pixel-operations are now mostly performed on the video-frames, which seem to be more thread-safe. 

**Ideas, reports, fixes most welcome.***

 Problems are described in more detail in the demos.
