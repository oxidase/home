/* -*- compile-command: "gcc test.c -lrt -pthread" -*- */
/*
No hang
./a.out 1
./a.out 2 &  ( sleep 1 ; ./a.out 3 )

Hangs on the third line with libc 2.25
./a.out 1
./a.out 2 &  p=$! ; ( sleep 1 ; kill -SIGABRT $p ; ./a.out 3 )
./a.out 2 &  ( sleep 1 ; ./a.out 3 )
*/


#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdlib.h>

const char mmap_name[] = "/tmp/test.mmap";

void* mmap_create(const char *name, size_t size)
{
    void * retv = NULL;
    mode_t origMask = umask(0);
    int mmapFd = open(name, O_CREAT|O_RDWR, 00666);
    umask(origMask);
    if (mmapFd < 0)
    {
        perror("open mmapFd failed");
        return NULL;
    }

    if ((ftruncate(mmapFd, size) == 0))
    {
        int result = lseek(mmapFd, size - 1, SEEK_SET);
        if (result == -1)
        {
            perror("lseek mmapFd failed");
            close(mmapFd);
            return NULL;
        }

        result = write(mmapFd, "", 1);
        if (result != 1)
        {
            perror("write mmapFd failed");
            close(mmapFd);
            return NULL;
        }
        retv  =  mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, mmapFd, 0);
        if (retv == MAP_FAILED || retv == NULL)
        {
            perror("mmap");
            close(mmapFd);
            return NULL;
        }
    }
    return retv;
}

void* mmap_open(const char *name, size_t size)
{
    void * retv = NULL;
    int mmapFd = open(name, O_RDWR, 00666);
    if (mmapFd < 0)
    {
        return NULL;
    }
    int result = lseek(mmapFd, 0, SEEK_END);
    if (result == -1)
    {
        perror("lseek mmapFd failed");
        close(mmapFd);
        return NULL;
    }
    if (result != size)
    {
        printf("The file has %d bytes\n", result);
        close(mmapFd);
        return NULL;
    }
    retv  =  mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, mmapFd, 0);

    if (retv == MAP_FAILED || retv == NULL)
    {
        perror("mmap");
        close(mmapFd);
        return NULL;
    }

    close(mmapFd);
    return retv;
}


typedef struct InternalData
{
  pthread_mutex_t mutex;
  pthread_cond_t condition;
} InternalData;

int main(int argc, char *argv[])
{
  if (argc == 2) {
    if (atoi(argv[1]) == 1) {
      InternalData* block = (InternalData*)mmap_create(mmap_name, sizeof(InternalData));

      // init the cond and mutex
      pthread_condattr_t cond_attr;
      pthread_condattr_init(&cond_attr);
      pthread_condattr_setpshared(&cond_attr, PTHREAD_PROCESS_SHARED);
      pthread_cond_init(&block->condition, &cond_attr);
      pthread_condattr_destroy(&cond_attr);

      pthread_mutexattr_t m_attr;
      pthread_mutexattr_init(&m_attr);
      pthread_mutexattr_setpshared(&m_attr, PTHREAD_PROCESS_SHARED);
      pthread_mutex_init(&block->mutex, &m_attr);
      pthread_mutexattr_destroy(&m_attr);

    } else if (atoi(argv[1]) == 2) {
      InternalData* block = (InternalData*)mmap_open(mmap_name, sizeof(InternalData));
      pthread_mutex_lock(&block->mutex);
      printf("waiting\n");
      pthread_cond_wait(&block->condition, &block->mutex);
      printf("received\n");
      pthread_mutex_unlock(&block->mutex);
    } else if (atoi(argv[1]) == 3) {
      InternalData* block = (InternalData*)mmap_open(mmap_name, sizeof(InternalData));
      pthread_mutex_lock(&block->mutex);
      printf("sending\n");
      pthread_cond_broadcast(&block->condition);
      printf("sent\n");
      pthread_mutex_unlock(&block->mutex);
    }
  }
  return 0;
}
