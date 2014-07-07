// -*- compile-command: "g++ -Wall -std=c++11 -O0 -g test_crypto.cpp -o test_crypto -lsodium" -*-

#include <string.h>
#include <sodium.h>

void print_hex_string(const char* msg, const uint8_t *bin_string, size_t length)
{   // free outside
    size_t i;
    printf("%s (%d) ", msg, length);
    for (i = 0; i < length; ++i)
        printf("%02X", bin_string[i]);
    printf("\n");
}


int main()
{
    const char  message[] = "Hello, Bob";
    const size_t mlength = sizeof(message);
    const char  signature[] = "Bob";
    const size_t slength = sizeof(signature);

    unsigned char public_key_a[crypto_box_PUBLICKEYBYTES] = {0}, public_key_b[crypto_box_PUBLICKEYBYTES] = {0};
    unsigned char secret_key_a[crypto_box_SECRETKEYBYTES] = {0}, secret_key_b[crypto_box_SECRETKEYBYTES] = {0};
    unsigned char enc_key_a[crypto_box_BEFORENMBYTES] = {0}, enc_key_b[crypto_box_BEFORENMBYTES] = {0};
    unsigned char nonce[crypto_box_NONCEBYTES] = {0};

    uint8_t plaintext[128 + crypto_box_MACBYTES + crypto_box_BOXZEROBYTES] = {0};
    uint8_t encrypted[128 + crypto_box_MACBYTES + crypto_box_BOXZEROBYTES] = {0};
    uint8_t received [128 + crypto_box_MACBYTES + crypto_box_BOXZEROBYTES] = {0};
    uint8_t decrypted[128 + crypto_box_MACBYTES + crypto_box_BOXZEROBYTES] = {0};

    crypto_box_keypair(public_key_a, secret_key_a);
    crypto_box_keypair(public_key_b, secret_key_b);

    print_hex_string("public_key A", public_key_a, sizeof(public_key_a));
    print_hex_string("secret_key A", secret_key_a, sizeof(secret_key_a));
    print_hex_string("public_key B", public_key_b, sizeof(public_key_b));
    print_hex_string("secret_key B", secret_key_b, sizeof(secret_key_b));

    randombytes_buf(nonce, crypto_box_NONCEBYTES);

    print_hex_string("nonce", nonce, sizeof(nonce));

    // Alice side
    memset(plaintext, 0, crypto_box_ZEROBYTES);
    memcpy(plaintext + crypto_box_ZEROBYTES, message, mlength);
    print_hex_string("message   ", plaintext, crypto_box_ZEROBYTES + mlength);
    if (crypto_box(encrypted, plaintext, crypto_box_ZEROBYTES + mlength, nonce, public_key_b, secret_key_a) != 0) {
        printf("crypto_box error\n");
        exit(1);
    }
    const size_t clength = mlength + crypto_box_MACBYTES;
    print_hex_string("encrypted ", crypto_box_BOXZEROBYTES + encrypted, clength);

    // Bob side
    memset(received, 0, crypto_box_BOXZEROBYTES);
    memcpy(received + crypto_box_BOXZEROBYTES, crypto_box_BOXZEROBYTES + encrypted, clength);
    if (crypto_box_open(decrypted, received, crypto_box_BOXZEROBYTES + clength, nonce, public_key_a, secret_key_b) != 0) {
        printf("crypto_box_open error\n");
        exit(1);
    }
    print_hex_string("decrypted ", decrypted + crypto_box_ZEROBYTES, clength - crypto_box_MACBYTES);
    printf("'%s'\n", decrypted + crypto_box_ZEROBYTES);



    // Alice side
    if (crypto_box_beforenm(enc_key_a, public_key_b, secret_key_a) != 0)  {
        printf("crypto_box_beforenm error\n");
        exit(1);
    }
    print_hex_string("enc_key A", enc_key_a, sizeof(enc_key_a));

    // Bob side
    if (crypto_box_beforenm(enc_key_b, public_key_a, secret_key_b) != 0)  {
        printf("crypto_box_beforenm error\n");
        exit(1);
    }
    print_hex_string("enc_key B", enc_key_b, sizeof(enc_key_b));

    // Alice side
    memset(plaintext, 0, crypto_box_ZEROBYTES);
    memcpy(plaintext + crypto_box_ZEROBYTES, message, mlength);
    print_hex_string("message   ", plaintext, crypto_box_ZEROBYTES + mlength);
    if (crypto_box_afternm(encrypted, plaintext, crypto_box_ZEROBYTES + mlength, nonce, enc_key_a) != 0) {
        printf("crypto_box_afternm error\n");
        exit(1);
    }
    print_hex_string("encrypted ", crypto_box_BOXZEROBYTES + encrypted, clength);

    memset(received, 0, crypto_box_BOXZEROBYTES);
    memcpy(received + crypto_box_BOXZEROBYTES, crypto_box_BOXZEROBYTES + encrypted, clength);
    if (crypto_box_open_afternm(decrypted, received, crypto_box_BOXZEROBYTES + clength, nonce, enc_key_b) != 0) {
        printf("crypto_box_open_afternm error\n");
        exit(1);
    }
    print_hex_string("decrypted ", decrypted + crypto_box_ZEROBYTES, clength - crypto_box_MACBYTES);
    printf("'%s'\n", decrypted + crypto_box_ZEROBYTES);

    return 0;
}
