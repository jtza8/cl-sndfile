(defpackage #:sndfile-cffi
  (:use #:cl #:cffi)
  (:export
   ; Formats Enum
   #:SF_FORMAT_WAV
   #:SF_FORMAT_AIFF
   #:SF_FORMAT_AU
   #:SF_FORMAT_RAW
   #:SF_FORMAT_PAF
   #:SF_FORMAT_SVX
   #:SF_FORMAT_NIST
   #:SF_FORMAT_VOC
   #:SF_FORMAT_IRCAM
   #:SF_FORMAT_W64
   #:SF_FORMAT_MAT4
   #:SF_FORMAT_MAT5
   #:SF_FORMAT_PVF
   #:SF_FORMAT_XI
   #:SF_FORMAT_HTK
   #:SF_FORMAT_SDS
   #:SF_FORMAT_AVR
   #:SF_FORMAT_WAVEX
   #:SF_FORMAT_SD2
   #:SF_FORMAT_FLAC
   #:SF_FORMAT_CAF
   #:SF_FORMAT_WVE
   #:SF_FORMAT_OGG
   #:SF_FORMAT_MPC2K
   #:SF_FORMAT_RF64
   #:SF_FORMAT_PCM_S8
   #:SF_FORMAT_PCM_16
   #:SF_FORMAT_PCM_24
   #:SF_FORMAT_PCM_32
   #:SF_FORMAT_PCM_U8
   #:SF_FORMAT_FLOAT
   #:SF_FORMAT_DOUBLE
   #:SF_FORMAT_ULAW
   #:SF_FORMAT_ALAW
   #:SF_FORMAT_IMA_ADPCM
   #:SF_FORMAT_MS_ADPCM
   #:SF_FORMAT_GSM610
   #:SF_FORMAT_VOX_ADPCM
   #:SF_FORMAT_G721_32
   #:SF_FORMAT_G723_24
   #:SF_FORMAT_G723_40
   #:SF_FORMAT_DWVW_12
   #:SF_FORMAT_DWVW_16
   #:SF_FORMAT_DWVW_24
   #:SF_FORMAT_DWVW_N
   #:SF_FORMAT_DPCM_8
   #:SF_FORMAT_DPCM_16
   #:SF_FORMAT_VORBIS
   #:SF_ENDIAN_FILE
   #:SF_ENDIAN_LITTLE
   #:SF_ENDIAN_BIG
   #:SF_ENDIAN_CPU
   #:SF_FORMAT_SUBMASK
   #:SF_FORMAT_TYPEMASK
   #:SF_FORMAT_ENDMASK
   ; Technical Enum
   #:SFC_GET_LIB_VERSION
   #:SFC_GET_LOG_INFO
   #:SFC_GET_CURRENT_SF_INFO
   #:SFC_GET_NORM_DOUBLE
   #:SFC_GET_NORM_FLOAT
   #:SFC_SET_NORM_DOUBLE
   #:SFC_SET_NORM_FLOAT
   #:SFC_SET_SCALE_FLOAT_INT_READ
   #:SFC_SET_SCALE_INT_FLOAT_WRITE
   #:SFC_GET_SIMPLE_FORMAT_COUNT
   #:SFC_GET_SIMPLE_FORMAT
   #:SFC_GET_FORMAT_INFO
   #:SFC_GET_FORMAT_MAJOR_COUNT
   #:SFC_GET_FORMAT_MAJOR
   #:SFC_GET_FORMAT_SUBTYPE_COUNT
   #:SFC_GET_FORMAT_SUBTYPE
   #:SFC_CALC_SIGNAL_MAX
   #:SFC_CALC_NORM_SIGNAL_MAX
   #:SFC_CALC_MAX_ALL_CHANNELS
   #:SFC_CALC_NORM_MAX_ALL_CHANNELS
   #:SFC_GET_SIGNAL_MAX
   #:SFC_GET_MAX_ALL_CHANNELS
   #:SFC_SET_ADD_PEAK_CHUNK
   #:SFC_SET_ADD_HEADER_PAD_CHUNK
   #:SFC_UPDATE_HEADER_NOW
   #:SFC_SET_UPDATE_HEADER_AUTO
   #:SFC_FILE_TRUNCATE
   #:SFC_SET_RAW_START_OFFSET
   #:SFC_SET_DITHER_ON_WRITE
   #:SFC_SET_DITHER_ON_READ
   #:SFC_GET_DITHER_INFO_COUNT
   #:SFC_GET_DITHER_INFO
   #:SFC_GET_EMBED_FILE_INFO
   #:SFC_SET_CLIPPING
   #:SFC_GET_CLIPPING
   #:SFC_GET_INSTRUMENT
   #:SFC_SET_INSTRUMENT
   #:SFC_GET_LOOP_INFO
   #:SFC_GET_BROADCAST_INFO
   #:SFC_SET_BROADCAST_INFO
   #:SFC_GET_CHANNEL_MAP_INFO
   #:SFC_SET_CHANNEL_MAP_INFO
   #:SFC_RAW_DATA_NEEDS_ENDSWAP
   #:SFC_WAVEX_SET_AMBISONIC
   #:SFC_WAVEX_GET_AMBISONIC
   #:SFC_SET_VBR_ENCODING_QUALITY
   #:SFC_TEST_IEEE_FLOAT_REPLACE
   #:SFC_SET_ADD_DITHER_ON_WRITE
   #:SFC_SET_ADD_DITHER_ON_READ
   ; ID3 Enum
   #:SF_STR_TITLE
   #:SF_STR_COPYRIGHT
   #:SF_STR_SOFTWARE
   #:SF_STR_ARTIST
   #:SF_STR_COMMENT
   #:SF_STR_DATE
   #:SF_STR_ALBUM
   #:SF_STR_LICENSE
   #:SF_STR_TRACKNUMBER
   #:SF_STR_GENRE
   ; Meta Enum
   #:SF_FALSE
   #:SF_TRUE
   #:SFM_READ
   #:SFM_WRITE
   #:SFM_RDWR
   #:SF_AMBISONIC_NONE
   #:SF_AMBISONIC_B_FORMAT
   ; Error Enum
   #:SF_ERR_NO_ERROR
   #:SF_ERR_UNRECOGNISED_FORMAT
   #:SF_ERR_SYSTEM
   #:SF_ERR_MALFORMED_FILE
   #:SF_ERR_UNSUPPORTED_ENCODING
   ; Channels Enum
   #:SF_CHANNEL_MAP_INVALID
   #:SF_CHANNEL_MAP_MONO
   #:SF_CHANNEL_MAP_LEFT
   #:SF_CHANNEL_MAP_RIGHT
   #:SF_CHANNEL_MAP_CENTER
   #:SF_CHANNEL_MAP_FRONT_LEFT
   #:SF_CHANNEL_MAP_FRONT_RIGHT
   #:SF_CHANNEL_MAP_FRONT_CENTER
   #:SF_CHANNEL_MAP_REAR_CENTER
   #:SF_CHANNEL_MAP_REAR_LEFT
   #:SF_CHANNEL_MAP_REAR_RIGHT
   #:SF_CHANNEL_MAP_LFE
   #:SF_CHANNEL_MAP_FRONT_LEFT_OF_CENTER
   #:SF_CHANNEL_MAP_FRONT_RIGHT_OF_CENTER
   #:SF_CHANNEL_MAP_SIDE_LEFT
   #:SF_CHANNEL_MAP_SIDE_RIGHT
   #:SF_CHANNEL_MAP_TOP_CENTER
   #:SF_CHANNEL_MAP_TOP_FRONT_LEFT
   #:SF_CHANNEL_MAP_TOP_FRONT_RIGHT
   #:SF_CHANNEL_MAP_TOP_FRONT_CENTER
   #:SF_CHANNEL_MAP_TOP_REAR_LEFT
   #:SF_CHANNEL_MAP_TOP_REAR_RIGHT
   #:SF_CHANNEL_MAP_TOP_REAR_CENTER
   #:SF_CHANNEL_MAP_AMBISONIC_B_W
   #:SF_CHANNEL_MAP_AMBISONIC_B_X
   #:SF_CHANNEL_MAP_AMBISONIC_B_Y
   #:SF_CHANNEL_MAP_AMBISONIC_B_Z
   #:SF_CHANNEL_MAP_MAX
   ; Define
   #:SF_COUNT_MAX
   ; Structs
   #:SF_INFO
   #:SF_FORMAT_INFO
   ; Misc Enum
   #:SFD_DEFAULT_LEVEL
   #:SFD_CUSTOM_LEVEL 
   #:SFD_NO_DITHER
   #:SFD_WHITE
   #:SFD_TRIANGULAR_PDF
   ; Structs
   #:SF_DITHER_INFO
   #:SF_EMBED_FILE_INFO
   ; Loop Enum
   #:SF_LOOP_NONE
   #:SF_LOOP_FORWARD
   #:SF_LOOP_BACKWARD
   #:SF_LOOP_ALTERNATING
   ; Structs
   #:SF_INSTRUMENTS
   #:SF_INSTRUMENTS_loops
   #:SF_LOOP_INFO
   #:SF_BROADCAST_INFO
   #:SF_VIRTUAL_IO
   ; Functions
   #:sf_open
   #:sf_open_fd
   #:sf_open_virtual
   #:sf_error
   #:sf_strerror
   #:sf_error_number
   #:sf_perror
   #:sf_error_str
   #:sf_command
   #:sf_format_check
   #:sf_seek
   #:sf_set_string
   #:sf_get_string
   #:sf_version_string
   #:sf_read_raw
   #:sf_write_raw
   #:sf_readf_short
   #:sf_writef_short
   #:sf_readf_int
   #:sf_writef_int
   #:sf_readf_float
   #:sf_writef_float
   #:sf_readf_double
   #:sf_writef_double
   #:sf_read_short
   #:sf_write_short
   #:sf_read_int
   #:sf_write_int
   #:sf_read_float
   #:sf_write_float
   #:sf_read_double
   #:sf_write_double
   #:sf_close
   #:sf_write_sync))

(defpackage #:sndfile
  (:use #:cl :sndfile-cffi)
  (:nicknames #:sf)
  (:shadow #:open)
  (:export
   #:open))