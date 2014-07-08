// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g sandbox_alice.cpp -o sandbox_alice -L../toxcore/build/.libs -Wl,-Bstatic -ltoxcore" -*-
// udp and udp.port == 33445 and data.data[0] != 21
#include <iostream>
#include <tox/tox.h>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "sandbox_shared.h"

int main()
{

    int ipv6 = 1;
    Tox *m = tox_new(ipv6);

    tox_callback_connection_status(m, on_connectionchange, NULL);
    tox_callback_typing_change(m, on_typing_change, NULL);
    tox_callback_friend_request(m, on_request, NULL);
    tox_callback_friend_message(m, on_message, NULL);
    tox_callback_name_change(m, on_nickchange, NULL);
    tox_callback_user_status(m, on_statuschange, NULL);
    tox_callback_status_message(m, on_statusmessagechange, NULL);
    tox_callback_friend_action(m, on_action, NULL);
    tox_callback_group_invite(m, on_groupinvite, NULL);
    tox_callback_group_message(m, on_groupmessage, NULL);
    tox_callback_group_action(m, on_groupaction, NULL);
    tox_callback_group_namelist_change(m, on_group_namelistchange, NULL);
    tox_callback_file_send_request(m, on_file_sendrequest, NULL);
    tox_callback_file_control(m, on_file_control, NULL);
    tox_callback_file_data(m, on_file_data, NULL);

    tox_set_name(m, (uint8_t *) "Alice", strlen("Alice"));

    load_data(m, "alice.data");

    uint8_t id[TOX_FRIEND_ADDRESS_SIZE];
    tox_get_address(m, id);
    printf("Alice id "); print_hex_string(id, TOX_FRIEND_ADDRESS_SIZE);
    if (FILE *fp = fopen("alice.id", "wb")) {
        fwrite(id, sizeof(id), 1, fp);
        fclose(fp);
    }

    // uint8_t *key_binary = hex_string_to_bin("951C88B7E75C867418ACDB5D273821372BB5BD652740BCDF623A4FA293E75D2F");
    // std::cout << "tox_bootstrap_from_address " <<  tox_bootstrap_from_address(m, "192.254.75.98", 1, htons(33445), key_binary) << "\n";

    uint8_t *key_binary = hex_string_to_bin("F815AB3C949A49A65FA9C2C4EADDD468A92543104ACDA172560108FA6ACD397F");
    std::cout << "tox_bootstrap_from_address " <<  tox_bootstrap_from_address(m, "127.0.0.1", 1, htons(33445), key_binary) << "\n";
    free(key_binary);

    bool is_connected = false;
    while (1) {
        tox_do(m);
        if (!is_connected && tox_isconnected(m)) {
            is_connected = true;
            std::cout << "connected\n";


        }
        usleep(10000);
    }
}
