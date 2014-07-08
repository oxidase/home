// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g -L/usr/local sandbox_alice.cpp -o sandbox_alice -ltoxcore; g++ -Wall -std=c++11 -O0 -g -L/usr/local sandbox_bob.cpp -o sandbox_bob -ltoxcore; " -*-

#ifndef SANDBOX_SHARED_H
#define SANDBOX_SHARED_H

#include <iostream>
#include <tox/tox.h>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdio.h>

void print_hex_string(const uint8_t *bin_string, size_t length)
{
    size_t i;
    for (i = 0; i < length; ++i)
        printf("%02X", bin_string[i]);
    printf("\n");
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


/*
 * Store Messenger to given location
 * Return 0 stored successfully
 * Return 1 file path is NULL
 * Return 2 malloc failed
 * Return 3 opening path failed
 * Return 4 fwrite failed
 */
int store_data(Tox *m, const char *path)
{
    if (path == NULL)
        return 1;

    FILE *fd;
    size_t len;
    uint8_t *buf;

    len = tox_size(m);
    buf = (uint8_t *)malloc(len);

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

void load_data(Tox *m, const char *path)
{
    FILE *fd;
    size_t len;
    uint8_t *buf;

    if ((fd = fopen(path, "rb")) != NULL) {
        fseek(fd, 0, SEEK_END);
        len = ftell(fd);
        fseek(fd, 0, SEEK_SET);

        buf = (uint8_t *)malloc(len);

        if (buf == NULL) {
            fclose(fd);
            fprintf(stderr, "failed in load_data");
            exit(1);
        }

        if (fread(buf, len, 1, fd) != 1) {
            free(buf);
            fclose(fd);
            fprintf(stderr, "failed in load_data");
            exit(1);
        }

        tox_load(m, buf, len);
        // load_friendlist(m);

        free(buf);
        fclose(fd);
    } else {
        int st;

        if ((st = store_data(m, path)) != 0) {
            fprintf(stderr, "failed in load_data");
            exit(1);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
// Tox callbacks
void on_connectionchange(Tox *m, int32_t friendnumber, uint8_t is_online, void *userdata)
{
    printf("%s: %d %d\n", __func__, friendnumber,  is_online);
}

void on_typing_change(Tox *m, int32_t friendnumber, uint8_t is_typing, void *userdata)
{
    printf("%s: %d %d\n", __func__, friendnumber,  is_typing);
}

void on_request(Tox *m, const uint8_t *public_key, const uint8_t *data, uint16_t length, void *userdata)
{
    printf("%s: (%d) %s from ", __func__, length, data); print_hex_string(public_key, TOX_FRIEND_ADDRESS_SIZE);
}

void on_message(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, friendnumber,  length, string);
}

void on_action(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, friendnumber, length, string);
}

void on_nickchange(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, friendnumber,  length, string);
}

void on_statusmessagechange(Tox *m, int32_t friendnumber, const uint8_t *string, uint16_t length, void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, friendnumber,  length, string);
}

void on_statuschange(Tox *m, int32_t friendnumber, uint8_t status, void *userdata)
{
    printf("%s: %d %d\n", __func__, friendnumber,  status);
}

void on_groupinvite(Tox *m, int32_t friendnumber, const uint8_t *group_pub_key, void *userdata)
{
    printf("%s: %d group_pub_key ", __func__, friendnumber); print_hex_string(group_pub_key, TOX_FRIEND_ADDRESS_SIZE);
}

void on_group_namelistchange(Tox *m, int groupnumber, int peernumber, uint8_t change, void *userdata)
{
    printf("%s: %d %d\n", __func__, peernumber,  change);
}

void on_groupmessage(Tox *m, int groupnumber, int peernumber, const uint8_t *message, uint16_t length, void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, peernumber, length, message);
}

void on_groupaction(Tox *m, int groupnumber, int peernumber, const uint8_t *action, uint16_t length,
                    void *userdata)
{
    printf("%s: %d (%d) %s\n", __func__, peernumber, length, action);
}

void on_file_sendrequest(Tox *m, int32_t friendnumber, uint8_t filenumber, uint64_t filesize,
                         const uint8_t *filename, uint16_t filename_length, void *userdata)
{
    printf("%s: %d %d (%d) %s, %llud bytes\n", __func__, friendnumber,  filenumber, filename_length, filename, filesize);
}

void on_file_control (Tox *m, int32_t friendnumber, uint8_t receive_send, uint8_t filenumber,
                      uint8_t control_type, const uint8_t *data, uint16_t length, void *userdata)
{
    printf("%s: %d %d %d %d\n", __func__, friendnumber,  receive_send, filenumber, control_type);
}


void on_file_data(Tox *m, int32_t friendnumber, uint8_t filenumber, const uint8_t *data, uint16_t length, void *userdata)
{
    printf("%s: %d %d %d\n", __func__, friendnumber, filenumber, length);
}

void on_av_invite(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
}

void on_av_start(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
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
}

void on_av_starting(int32_t call_index, void*)
{
    printf("%s %d\n",  __func__, call_index);
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

#endif
