// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g playback_resampler.cpp -o playback_resampler $(pkg-config --cflags --libs sndfile samplerate libpulse-simple)" -*-
#include <stdio.h>
#include <sys/ioctl.h>

#include <sndfile.h>
#include <samplerate.h>

#include <pulse/pulseaudio.h>
#include <pulse/simple.h>

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s [filename]\n", argv[0]);
        return 0;
    }

    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);

    SNDFILE *f;
    SF_INFO info;
    SRC_DATA    src_data;

    f = sf_open(argv[1], SFM_READ, &info);
    int res = sf_error(f);

    if (res != 0) {
        printf("File error: %s\n", sf_strerror(f));
        return 0;
    }

    pa_simple *s;
    pa_sample_spec ss;

    ss.format = PA_SAMPLE_FLOAT32NE;
    ss.channels = info.channels;
    ss.rate = 48000;

    s = pa_simple_new(NULL, "Nexus", PA_STREAM_PLAYBACK, NULL, "Music", &ss, NULL, NULL, NULL);

    double duration;
    duration = (1.0 * info.frames) / info.samplerate;

    printf("Sample rate: %iHz\n", info.samplerate);
    printf("Channels: %i\n", info.channels);
    printf("Duration: %fs\n", duration);

    int frames_in_star = info.frames / (w.ws_col-1), read_frames = 0, printed_frames = 0;
    int done = 0;
    int err;

    const int nframes = 1024;
    printf("Buffer length: %dms\n", 1000 * nframes / info.samplerate);

    src_data.src_ratio = (double)ss.rate / (double)info.samplerate;
    src_data.output_frames = 2*nframes * ss.rate / info.samplerate + 1;
    src_data.data_in = (float*) malloc(nframes * info.channels * sizeof(float));
    src_data.data_out = (float*) malloc(src_data.output_frames * info.channels * sizeof(float));

    while (!done) {
        int count = sf_readf_float(f, src_data.data_in, nframes);

        read_frames += count;
        while (printed_frames < read_frames) {
            printed_frames += frames_in_star;
            printf("*"); fflush(stdout);
        }

        src_data.input_frames = count;
        if ((err = src_simple (&src_data, SRC_SINC_BEST_QUALITY, info.channels))) {
            fprintf(stderr, "src_simple failed: %s\n", src_strerror(err));
            break;
        }

        if (pa_simple_write(s, src_data.data_out, (src_data.output_frames_gen * info.channels * sizeof(float)), &err) < 0) {
            fprintf(stderr, "pa_simple_write failed: %s\n", pa_strerror(err));
            break;
        }

        if (count < nframes)
            done = 1;
    }

    free(src_data.data_in);
    free(src_data.data_out);

    sf_close(f);

    pa_simple_free(s);

    return 0;
}
