// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g test_toxav.cpp -o test_toxav -ltoxav -ltoxcore $(pkg-config --cflags --libs libpulse-simple) -pthread" -*-
// udp and udp.port == 33445 and data.data[0] != 21
#include <iostream>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <thread>
#include <atomic>

#include <tox/tox.h>
#include <tox/toxav.h>

#include <pulse/simple.h>

#define ALICE_DATA_ID (0)
#define BOB_DATA_ID (1)

const char* person_names[] = { "Alice", "Bob" };

int store_data(Tox *m, const char *path)
{
    if (path == NULL)
        return 1;

    FILE *fd;
    size_t len;
    uint8_t *buf;

    len = tox_size(m);
    buf = (uint8_t*)malloc(len);

    if (buf == NULL)
        return 2;

    tox_save(m, buf);

    fd = fopen(path, "wb");

    if (fd == NULL) {
        free(buf);
        return 3;
    }

    if (fwrite(buf, len, 1, fd) != 1) {
        free(buf);
        fclose(fd);
        return 4;
    }

    free(buf);
    fclose(fd);
    return 0;
}

static void load_data(Tox *m, const char *path)
{
    FILE *fd;
    size_t len;
    uint8_t *buf;

    if ((fd = fopen(path, "rb")) != NULL) {
        fseek(fd, 0, SEEK_END);
        len = ftell(fd);
        fseek(fd, 0, SEEK_SET);

        buf = (uint8_t*)malloc(len);

        if (buf == NULL) {
            fclose(fd);
            printf("failed in load_data\n");
            return;
        }

        if (fread(buf, len, 1, fd) != 1) {
            free(buf);
            fclose(fd);
            printf("failed in load_data\n");
            return;
        }

        tox_load(m, buf, len);
        //load_friendlist(m);

        free(buf);
        fclose(fd);
    } else {
        int st;

        if ((st = store_data(m, path)) != 0) {
            printf("failed in load_data\n");
            return;
        }
    }
}

uint8_t *hex_string_to_bin(const char *hex_string)
{   // free outside
    size_t len = strlen(hex_string);
    uint8_t *val = (uint8_t*)malloc(len);
    size_t i;
    for (i = 0; i < len; ++i, hex_string += 2)
        sscanf(hex_string, "%2hhx", &val[i]);
    return val;
}

uint8_t *bin_to_hex_string(const uint8_t *bin_string, size_t length)
{   // free outside
    uint8_t *res = (uint8_t*)malloc(2*length + 1), *hex_string = res;
    size_t i;
    for (i = 0; i < length; ++i, hex_string += 2)
        sprintf((char*)hex_string, "%02hhx", bin_string[i]);
    *hex_string = 0;
    return res;
}

void print_hex_string(const uint8_t *bin_string, size_t length)
{
    size_t i;
    for (i = 0; i < length; ++i)
        printf("%02X", bin_string[i]);
    printf("\n");
}

////////////////////////////////////////////////////////////////////////////////
// PulseAudio capture and playback
pa_simple *pulse_in = 0, *pulse_out = 0;

static const pa_sample_spec pulse_ss = {
    .format = PA_SAMPLE_S16LE,
    .rate = 44100,
    .channels = 2
};

std::atomic<bool> playback_started(false);
void playback_thread_func(ToxAv *av) {
    int16_t dest[65536];
    printf("%s: started %p\n", __func__, av);
    while (1) {
        if (playback_started) {
            int ret = toxav_recv_audio(av, 0, sizeof(dest)/sizeof(dest[0]), dest);
            if (ret < 0) {
                fprintf(stderr, "%s: ToxAvError %d\n", __func__, ret);
            } else if (ret > 0) {
                printf("%s: received %d frames\n", __func__, ret);
            }
        }
        usleep(1000);
    }
}



////////////////////////////////////////////////////////////////////////////////
// test state machine
typedef void *(*state_func_t)(Tox*, ToxAv *, const uint8_t*);
typedef state_func_t (*state_func_p)(Tox*, ToxAv *, const uint8_t*);

state_func_p state_alice_start(Tox* m, ToxAv* av, const uint8_t* friend_id);
state_func_p state_bob_start(Tox* m, ToxAv* av, const uint8_t* friend_id);
state_func_p state_wait(Tox*, ToxAv*, const uint8_t*);
state_func_p state_finished(Tox*, ToxAv*, const uint8_t*);

state_func_p alice_state = (state_func_p)state_alice_start;
state_func_p bob_state = (state_func_p)state_bob_start;

state_func_p state_alice_start(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    // return (state_func_p)state_wait;
    if (tox_isconnected(m)) {
        printf("Alice connected\n");
        return (state_func_p)state_wait;
    }
    return (state_func_p)state_alice_start;
}

state_func_p state_alice_accept_request(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    tox_add_friend_norequest(m, friend_id);
    return (state_func_p)state_wait;
}

state_func_p state_alice_call_bob(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    int32_t call_index;
    toxav_call(av, &call_index, 0, TypeAudio, 5);
    return (state_func_p)state_wait;
}

state_func_p state_alice_start_audio(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    int16_t frame[2*2880] = {5};
    uint8_t encoded[16536];
    ToxAvCodecSettings cs;
    memcpy(&cs, &av_DefaultSettings, sizeof(ToxAvCodecSettings));

    std::cout<< "toxav_prepare_transmission " << toxav_prepare_transmission(av, 0, &cs, 0) << std::endl;;
    int rc = toxav_prepare_audio_frame(av, 0, encoded, sizeof(encoded), frame, sizeof(frame)/sizeof(frame[0])/2);
    std::cout << "toxav_prepare_audio_frame " << rc << std::endl;
    std::cout<< "toxav_send_audio " << toxav_send_audio(av, 0, encoded, rc) << std::endl;
    std::cout<< "toxav_send_audio " << toxav_send_audio(av, 0, encoded, rc) << std::endl;
    std::cout<< "toxav_send_audio " << toxav_send_audio(av, 0, encoded, rc) << std::endl;
    std::cout<< "toxav_send_audio " << toxav_send_audio(av, 0, encoded, rc) << std::endl;

    return (state_func_p)state_wait;
}

state_func_p state_bob_start(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    // return (state_func_p)state_wait;
    if (tox_isconnected(m)) {
        printf("Bob connected\n");
        const uint8_t msg[] = "Hello, Alice", status[] = "雨ですね。。。";
        tox_add_friend(m, friend_id, msg, sizeof(msg));
        tox_set_user_status(m, TOX_USERSTATUS_NONE);
        tox_set_status_message(m, status, sizeof(status));
        return (state_func_p)state_wait;
    }
    return (state_func_p)state_bob_start;
}

state_func_p state_bob_answer_to_alice(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    toxav_answer(av, 0, TypeAudio);
    ToxAvCodecSettings cs;
    memcpy(&cs, &av_DefaultSettings, sizeof(ToxAvCodecSettings));

    std::cout<< "bob toxav_prepare_transmission " << toxav_prepare_transmission(av, 0, &cs, 0) << std::endl;;
    playback_started = true;

    return (state_func_p)state_wait;
}

state_func_p state_bob_starting_audio(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    return (state_func_p)state_wait;
}

state_func_p state_wait(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    return (state_func_p)state_wait;
}

state_func_p state_finished(Tox* m, ToxAv* av, const uint8_t* friend_id)
{
    return (state_func_p)state_finished;
}

////////////////////////////////////////////////////////////////////////////////
// Tox callbacks
void on_connectionchange(Tox *m, int32_t friendnumber, uint8_t is_online, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d\n", person_names[data_id], __func__, friendnumber,  is_online);
    if (data_id == ALICE_DATA_ID)
        alice_state = (state_func_p)state_alice_call_bob;
}

void on_typing_change(Tox *m, int32_t friendnumber, uint8_t is_typing, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d\n", person_names[data_id], __func__, friendnumber,  is_typing);
}

void on_request(Tox *m, const uint8_t *public_key, const uint8_t *data, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: (%d) %s from ", person_names[data_id], __func__, length, data); print_hex_string(public_key, TOX_FRIEND_ADDRESS_SIZE);
    if (data_id == ALICE_DATA_ID)
        alice_state = (state_func_p)state_alice_accept_request;
}

void on_message(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, friendnumber,  length, string);
}

void on_action(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, friendnumber, length, string);
}

void on_nickchange(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, friendnumber,  length, string);
}

void on_statusmessagechange(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, friendnumber,  length, string);
}

void on_statuschange(Tox *m, int32_t friendnumber, uint8_t status, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d\n", person_names[data_id], __func__, friendnumber,  status);
}

void on_groupinvite(Tox *m, int32_t friendnumber, const uint8_t *group_pub_key, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d group_pub_key ", person_names[data_id], __func__, friendnumber); print_hex_string(group_pub_key, TOX_FRIEND_ADDRESS_SIZE);
}

void on_group_namelistchange(Tox *m, int groupnumber, int peernumber, uint8_t change, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d\n", person_names[data_id], __func__, peernumber,  change);
}

void on_groupmessage(Tox *m, int groupnumber, int peernumber, const uint8_t *message, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, peernumber, length, message);
}

void on_groupaction(Tox *m, int groupnumber, int peernumber, const uint8_t *action, uint16_t length,
                    void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d (%d) %s\n", person_names[data_id], __func__, peernumber, length, action);
}

void on_file_sendrequest(Tox *m, int32_t friendnumber, uint8_t filenumber, uint64_t filesize,
                         const uint8_t *filename, uint16_t filename_length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d (%d) %s, %llud bytes\n", person_names[data_id], __func__, friendnumber,  filenumber, filename_length, filename, filesize);
}

void on_file_control (Tox *m, int32_t friendnumber, uint8_t receive_send, uint8_t filenumber,
                      uint8_t control_type, const uint8_t *data, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d %d %d\n", person_names[data_id], __func__, friendnumber,  receive_send, filenumber, control_type);
}


void on_file_data(Tox *m, int32_t friendnumber, uint8_t filenumber, const uint8_t *data, uint16_t length, void *userdata)
{
    size_t data_id = (size_t)userdata;
    printf("%-6s %s: %d %d %d\n", person_names[data_id], __func__, friendnumber, filenumber, length);
}

void on_av_invite(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_start(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
    alice_state = (state_func_p)state_alice_start_audio;
}

void on_av_cancel(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_reject(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_end(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_ringing(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
    bob_state = (state_func_p)state_bob_answer_to_alice;
}

void on_av_starting(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
    bob_state = (state_func_p)state_bob_starting_audio;
}

void on_av_ending(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_error(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_request_timeout(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_peer_timeout(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void setup_tox_messenger(Tox *m, uint8_t *id, size_t person_data_id)
{
    tox_callback_connection_status(m, on_connectionchange, (void*)person_data_id);
    tox_callback_typing_change(m, on_typing_change, (void*)person_data_id);
    tox_callback_friend_request(m, on_request, (void*)person_data_id);
    tox_callback_friend_message(m, on_message, (void*)person_data_id);
    tox_callback_name_change(m, on_nickchange, (void*)person_data_id);
    tox_callback_user_status(m, on_statuschange, (void*)person_data_id);
    tox_callback_status_message(m, on_statusmessagechange, (void*)person_data_id);
    tox_callback_friend_action(m, on_action, (void*)person_data_id);
    tox_callback_group_invite(m, on_groupinvite, (void*)person_data_id);
    tox_callback_group_message(m, on_groupmessage, (void*)person_data_id);
    tox_callback_group_action(m, on_groupaction, (void*)person_data_id);
    tox_callback_group_namelist_change(m, on_group_namelistchange, (void*)person_data_id);
    tox_callback_file_send_request(m, on_file_sendrequest, (void*)person_data_id);
    tox_callback_file_control(m, on_file_control, (void*)person_data_id);
    tox_callback_file_data(m, on_file_data, (void*)person_data_id);

    tox_get_address(m, id);

    if (person_data_id < sizeof(person_names) / sizeof(person_names[0])) {
        tox_set_name(m, (uint8_t*)person_names[person_data_id], strlen(person_names[person_data_id]) + 1);
        printf("%-6s", person_names[person_data_id]); print_hex_string(id, TOX_FRIEND_ADDRESS_SIZE);
    } else {
        printf("id: "); print_hex_string(id, TOX_FRIEND_ADDRESS_SIZE);
    }
}


int main(int argc, char *argv[])
{
    if (!(pulse_in = pa_simple_new(NULL, argv[0], PA_STREAM_RECORD, NULL, "record", &pulse_ss, NULL, NULL, NULL))) {
        fprintf(stderr, "pa_simple_new() failed\n");
        return 1;
    }

    int ipv6 = 1;
    Tox *alice = tox_new(ipv6), *bob = tox_new(ipv6);
    uint8_t alice_id[TOX_FRIEND_ADDRESS_SIZE], bob_id[TOX_FRIEND_ADDRESS_SIZE];

    load_data(alice, "alice.data");
    load_data(bob, "bob.data");

    setup_tox_messenger(alice, alice_id, ALICE_DATA_ID);
    setup_tox_messenger(bob, bob_id, BOB_DATA_ID);

    ToxAv *alice_av = toxav_new(alice, 8);
    ToxAv *bob_av = toxav_new(bob, 8);

    toxav_register_callstate_callback (on_av_invite, av_OnInvite, NULL);
    toxav_register_callstate_callback (on_av_start, av_OnStart, NULL);
    toxav_register_callstate_callback (on_av_cancel, av_OnCancel, NULL);
    toxav_register_callstate_callback (on_av_reject, av_OnReject, NULL);
    toxav_register_callstate_callback (on_av_end, av_OnEnd, NULL);
    toxav_register_callstate_callback (on_av_ringing, av_OnRinging, NULL);
    toxav_register_callstate_callback (on_av_starting, av_OnStarting, NULL);
    toxav_register_callstate_callback (on_av_ending, av_OnEnding, NULL);
    toxav_register_callstate_callback (on_av_error, av_OnError, NULL);
    toxav_register_callstate_callback (on_av_request_timeout, av_OnRequestTimeout, NULL);
    toxav_register_callstate_callback (on_av_peer_timeout, av_OnPeerTimeout, NULL);

    uint8_t *key_binary = 0;
    const char *bootstrap_ip = "127.0.0.1";
    uint16_t bootstrap_port = 33445;
    if (argc > 3)
        bootstrap_port = atoi(argv[3]);
    if (argc > 2)
        bootstrap_ip = argv[2];
    if (argc > 1)
        key_binary = hex_string_to_bin(argv[1]);
    else
        key_binary = hex_string_to_bin("F815AB3C949A49A65FA9C2C4EADDD468A92543104ACDA172560108FA6ACD397F");

    printf("bootstrap node %s:%d, key ", bootstrap_ip, bootstrap_port); print_hex_string(key_binary, TOX_CLIENT_ID_SIZE);

    if ( !tox_bootstrap_from_address(alice, bootstrap_ip, ipv6, htons(bootstrap_port), key_binary) ||
         !tox_bootstrap_from_address(bob, bootstrap_ip, ipv6, htons(bootstrap_port), key_binary)) {
        fprintf(stderr, "tox_bootstrap_from_address failed\n");
        return 1;
    }
    free(key_binary);

    std::thread playback_thread(playback_thread_func, bob_av);

    while (alice_state != (state_func_p)state_finished || bob_state != (state_func_p)state_finished) {
        tox_do(alice);
        tox_do(bob);

        alice_state = (state_func_p)(*alice_state)(alice, alice_av, bob_id);
        bob_state = (state_func_p)(*bob_state)(bob, bob_av, alice_id);

        usleep(50*1000);

    }

    toxav_kill(alice_av);
    toxav_kill(bob_av);

    tox_kill(alice);
    tox_kill(bob);

    return 0;
}
