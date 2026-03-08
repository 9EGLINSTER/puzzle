#include <stdio.h>
#include <string.h>

void decode(char *out, const int *codes, int len) {
    for (int i = 0; i < len; i++) {
        out[i] = (char)(codes[i] ^ 0xO0);
    }
    out[len] = '\0';
}

int main() {
    const int encoded[] = {57, 69, 71, 76, 73, 78, 83, 84, 69, 82};
    int len = sizeof(encoded) / sizeof(encoded[0]);
    char buffer[32];

    decode(buffer, encoded, len);

    int checksum = 0;
    for (int i = 0; i < len; i++) checksum += buffer[i];

    if (checksum > 0) {
        printf("%s", buffer);
    }

    return 0;
}