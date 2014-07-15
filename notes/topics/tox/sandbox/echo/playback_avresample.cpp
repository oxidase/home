// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g playback_avresample.cpp -o playback_avresample $(pkg-config --cflags --libs sndfile libavresample libavutil libpulse-simple)" -*-
#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <sndfile.h>
extern "C" {
#include <libavresample/avresample.h>
#include <libavutil/opt.h>
#include <libavutil/samplefmt.h>
#include <libavutil/mathematics.h>
#include <libavutil/mem.h>
}

#include <pulse/pulseaudio.h>
#include <pulse/simple.h>

int main(int argc, char* argv[])
{
    char c;
    bool resample = true;
    int output_sample_rate = 48000;
    int output_channels = 2;
    const char *filename = "sounds/hello.ogg";
    while ( (c = getopt(argc, argv, "nrs:c:")) != -1) {
        printf("%c %d\n", c, optind);
        switch (c) {
        case 'r':
            resample = true;
            break;
        case 'n':
            resample = false;
            break;
        case 's':
            output_sample_rate = atoi(argv[optind]);
            break;
        case 'c':
            output_channels = atoi(optarg);
            output_channels = output_channels == 0 ? 1 : output_channels;
            break;
        case ':':
            break;
        }
    }
    if (optind < argc)
        filename = argv[optind];

    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);

    SNDFILE *f;
    SF_INFO info;
    AVAudioResampleContext *avr = 0;

    f = sf_open(filename, SFM_READ, &info);
    int res = sf_error(f);

    if (res != 0) {
        printf("File error: %s\n", sf_strerror(f));
        return 0;
    }

    pa_simple *s;
    pa_sample_spec ss;

    ss.format = PA_SAMPLE_FLOAT32NE;
    if (resample) {
        ss.rate = output_sample_rate;
        ss.channels = output_channels;
    } else {
        ss.rate = info.samplerate;
        ss.channels = info.channels;
    }

    s = pa_simple_new(NULL, "Nexus", PA_STREAM_PLAYBACK, NULL, "Music", &ss, NULL, NULL, NULL);

    uint64_t input_channel_layout = av_get_default_channel_layout(info.channels);
    uint64_t output_channel_layout = av_get_default_channel_layout(ss.channels);
    char input_layout_name[1024], output_layout_name[1024];
    av_get_channel_layout_string(input_layout_name, sizeof(input_layout_name), info.channels, input_channel_layout);
    av_get_channel_layout_string(output_layout_name, sizeof(output_layout_name), ss.channels, output_channel_layout);

    printf("Input sample rate: %iHz\n", info.samplerate);
    printf("Input channels: %i (%s)\n", info.channels, input_layout_name);
    printf("Output sample rate: %iHz\n", ss.rate);
    printf("Output channels: %i (%s)\n", ss.channels, output_layout_name);
    printf("Duration: %fs\n", (1.0 * info.frames) / info.samplerate);
    printf("Format: %08X\n", info.format);

    int samples_in_star = info.frames / ((w.ws_col > 1) ? (w.ws_col-1) : 80);
    int read_samples = 0, printed_samples = 0;
    int done = 0;
    int err;

    const int nsamples = 1024;
    printf("Buffer length: %dms\n", 1000 * nsamples / info.samplerate);

    if (!(avr = avresample_alloc_context())) {
        fprintf(stderr, "avresample_alloc_context failed\n");
        return 1;
    }

    av_opt_set_int(avr, "in_channel_layout", input_channel_layout, 0);
    av_opt_set_int(avr, "out_channel_layout", output_channel_layout, 0);
    av_opt_set_int(avr, "in_sample_rate", info.samplerate, 0);
    av_opt_set_int(avr, "out_sample_rate", ss.rate, 0);
    av_opt_set_int(avr, "in_sample_fmt", AV_SAMPLE_FMT_FLT, 0);
    av_opt_set_int(avr, "out_sample_fmt", AV_SAMPLE_FMT_FLT, 0);
    av_opt_set_int(avr, "internal_sample_fmt", AV_SAMPLE_FMT_FLTP, 0);

    if (avresample_open(avr)) {
        fprintf(stderr, "avresample_open failed\n");
        return 1;
    }

    uint8_t *data_in = (uint8_t*) malloc(nsamples * info.channels * sizeof(float));
    uint8_t *data_out = 0;
    int out_samples = av_rescale_rnd(2 * nsamples, ss.rate, info.samplerate, AV_ROUND_UP), out_linesize = 0;
    av_samples_alloc(&data_out, &out_linesize, ss.channels, out_samples, AV_SAMPLE_FMT_FLT, 0);

    while (!done) {
        int in_samples = sf_readf_float(f, (float*)data_in, nsamples);

        read_samples += in_samples;
        while (printed_samples < read_samples) {
            printed_samples += samples_in_star;
            printf("*"); fflush(stdout);
        }

        if (resample) {
            out_samples = avresample_convert(avr, &data_out, out_linesize, out_samples, &data_in, 0, in_samples);

            if (pa_simple_write(s, data_out, (ss.channels * out_samples * sizeof(float)), &err) < 0) {
                fprintf(stderr, "pa_simple_write failed: %s\n", pa_strerror(err));
                break;
            }
        } else {
            if (pa_simple_write(s, data_in, (in_samples * info.channels * sizeof(float)), &err) < 0) {
                fprintf(stderr, "pa_simple_write failed: %s\n", pa_strerror(err));
                break;
            }
        }

        if (in_samples < nsamples)
            done = 1;
    }

    avresample_close(avr);
    avresample_free(&avr);

    free(data_in);

    if (data_out)
        av_freep(&data_out);

    sf_close(f);

    pa_simple_free(s);

    return 0;
}
