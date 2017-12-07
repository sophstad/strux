#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

// for swapping values in array
void swap(int *x, int *y)
{
    // intermediate place holder variable
    int z;

    // allocate value of x to z, set x to y and y to z
    z = *x;
    *x = *y;
    *y = z;
    return ;
}

// for swapping values in array
void fswap(float *x, float *y)
{
    // intermediate place holder variable
    float z;

    // allocate value of x to z, set x to y and y to z
    z = *x;
    *x = *y;
    *y = z;
    return ;
}

// prints out int list
void display(int a[]) {
    int i;
    int n=0;
    int p = -1;
    while (p!= '\0')
    {
        n++;
        p = a[n];
    }

    printf("[");

    for(i = 0;i<n;i++) {
      printf("%d ",a[i]);
  }

  printf("]\n");
}

// prints out float list
void fdisplay(float a[]) {
    int i;
    int n=0;
    int p = -1;
    while (p!= '\0')
    {
        n++;
        p = a[n];
    }

    printf("[");

    for(i = 0;i<n;i++) {
      printf("%f ",a[i]);
  }

  printf("]\n");
}

// Function that uses median of three partitioning
int median_of_3(int a[], int left, int right)
{
    // get middle of array
    int middle = (left + right)/2;

    // rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        swap(&a[left],&a[middle]);
    }
    if (a[right] < a[left]) {
        swap(&a[left],&a[right]);
    }
    if (a[right]< a[middle]) {
        swap(&a[middle],&a[right]);
    }

    swap(&a[middle], &a[right - 1]);
    return a[right - 1];
}

float fmedian_of_3(float a[], int left, int right)
{
// get middle of array
    int middle = (left + right)/2;

// rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        fswap(&a[left],&a[middle]);
    }
    if (a[right] < a[left]) {
        fswap(&a[left],&a[right]);
    }
    if (a[right] < a[middle]) {
        fswap(&a[middle],&a[right]);
    }

    fswap(&a[middle], &a[right - 1]);
    return a[right - 1];
}

void quickSort(int a[], int left, int right)
{
  if (left < right) {
    int pivot = median_of_3(a,left,right);
    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
        while(a[++i]<pivot) {}
            while(pivot<a[--j]) {}
                if ( i < j) {
                    swap(&a[i],&a[j]);
                } else {
                    break ;
                }
            }
            swap(&a[i],& a[right -1]);
            quickSort(a,left,i-1);
            quickSort(a,i+1,right);
        }

        return ;
    }

void fquickSort(float a[], int left, int right)
{
  if (left < right) {
    int pivot = fmedian_of_3(a,left,right);

    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
        while(a[++i]<pivot) {}
            while(pivot<a[--j]) {}
                if ( i < j) {
                    fswap(&a[i],&a[j]);
                }
                else {
                    break ;
                }
            }
            fswap(&a[i], &a[right -1]);
            fquickSort(a,left,i-1);
            fquickSort(a,i+1,right);
        }

        return ;
    }
// ======== show quicksorts =============

int partition(int a[], int left, int right)
{
    int x = a[right];
    int i = left-1;
    for(int j=left; j<right; j++)
    {
        if(a[j] <= x)
        {
            i++;
      swap(&a[i], &a[j]);
      if (a[i] != a[j] && i != j) {
        printf("numbers swapped: %d,%d\n", a[j],a[i]);
        printf("array after swap: ");
        display(a);
      }
        }
    }
  swap(&a[i+1], &a[right]);
  if (a[i+1] != a[right] && ((i+1) != right)) {
    printf("numbers swapped: %d,%d\n", a[i+1],a[right]);
    printf("array after swap: ");
    display(a);
  }
    return i+1;
}

float fpartition(float a[], int left, int right)
{
    float x = a[right];
    int i = left-1;
    for(int j=left; j<right; j++)
    {
        if(a[j] <= x)
        {
            i++;
      fswap(&a[i], &a[j]);
      if (a[i] != a[j] && i != j) {
        printf("numbers swapped: %f,%f\n", a[j],a[i]);
        printf("array after swap: ");
        fdisplay(a);
      }
        }
    }
  fswap(&a[i+1], &a[right]);
  if (a[i+1] != a[right] && ((i+1) != right)) {
    printf("numbers swapped: %f,%f\n", a[i+1],a[right]);
    printf("array after swap: ");
    fdisplay(a);
  }
    return i+1;
}

int show_median_of_3(int a[], int left, int right)
{
    // get middle of array
    int middle = (left + right)/2;
    printf("============================================================\n");
    printf("At this step:\n");
    printf("current array:");
    display(a);

    // rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        swap(&a[left],&a[middle]);
        printf("pivot swapped: %d,%d\n", a[middle],a[left]);
        printf("array after swap: ");
        display(a);
    }
    if (a[right] < a[left]) {
        swap(&a[left],&a[right]);
        printf("pivot swapped: %d,%d\n", a[right],a[left]);
        printf("array after swap: ");
        display(a);
    }
    if (a[right]< a[middle]) {
        swap(&a[middle],&a[right]);
        printf("pivot swapped: %d,%d\n", a[right],a[middle]);
        printf("array after swap: ");
        display(a);
    }

    swap(&a[middle], &a[right - 1]);
    if (a[middle] != a[right-1] && (middle != (right-1))) {
      printf("numbers swapped: %d,%d\n", a[middle],a[right-1]);
      printf("array after swap: ");
      display(a);
    }
    return a[right - 1];
}

float show_fmedian_of_3(float a[], int left, int right)
{
// get middle of array
    int middle = (left + right)/2;
    printf("============================================================\n");
    printf("At this step:\n");
    printf("current array:");
    fdisplay(a);

// rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        fswap(&a[left],&a[middle]);
        printf("pivot swapped: %f,%f\n", a[middle],a[left]);
        printf("array after swap: ");
        fdisplay(a);
    }
    if (a[right] < a[left]) {
        fswap(&a[left],&a[right]);
        printf("pivot swapped: %f,%f\n", a[right],a[left]);
        printf("array after swap: ");
        fdisplay(a);
    }
    if (a[right] < a[middle]) {
        fswap(&a[middle],&a[right]);
        printf("pivot swapped: %f,%f\n", a[right],a[middle]);
        printf("array after swap: ");
        fdisplay(a);
    }

    fswap(&a[middle], &a[right - 1]);
    if (a[middle] != a[right-1] && (middle != (right-1))) {
      printf("numbers swapped: %f,%f\n", a[middle],a[right-1]);
      printf("array after swap: ");
      fdisplay(a);
    }
    return a[right - 1];
}

void showQuickSort(int a[], int left, int right)
{
    if(left < right)
    {
        show_median_of_3(a, left, right);

        int q = partition(a, left, right);
        showQuickSort(a, left, q-1);
        showQuickSort(a, q+1, right);
    }
}

void showfQuickSort(float a[], int left, int right)
{
    if(left < right)
    {
        show_fmedian_of_3(a, left, right);

        int q = fpartition(a, left, right);
        showfQuickSort(a, left, q-1);
        showfQuickSort(a, q+1, right);
    }
}

void cShowQuickSort(int a[]) {
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
      n++;
      p = a[n];
  }
  n = n - 1;
  showQuickSort(a, 0, n);
}

void cShowfQuickSort(float a[]) {
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
      n++;
      p = a[n];
  }
  n = n - 1;
  showfQuickSort(a, 0, n);
}

void cQuickSort(int a[]) {
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
      n++;
      p = a[n];
  }
  n = n - 1;
  quickSort(a, 0, n);
}

void cQuickfSort(float a[]) {
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
      n++;
      p = a[n];
  }
  n = n - 1;
  fquickSort(a, 0, n);
}

int main()
{
    // int a[] = {10, 35, 25, 56, 2002, 100, 90, 86, 40, 50, 7};
    // cShowQuickSort(a);
    // cQuickSort(a);

    // int b[] = {85,331,234,46,4,3,22,89,7,12, 33, 44,55,66,77};
    // cShowfQuickSort(b);
    // cQuickSort(b);

    // float c[] = { 3.4, 56.2, 5.3, 6.0, 2.6, 566.7, 778.3};
    // cShowfQuickSort(c);
    // cQuickfSort(c);

    // float d[] = { 55.4, 36.2, 23.3, 16.0, 52.6, 26.7, 78.3};
    // cShowfQuickSort(d);
    // cQuickfSort(d);

    // int f[] = {10, 100, 30, 90, 40, 50, 70};
    // showQuickSort(f, 0, 6);
    // cQuickSort(f);

}
