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
void fswap(double *x, double *y)
{
    // intermediate place holder variable
    double z;

    // allocate value of x to z, set x to y and y to z
    z = *x;
    *x = *y;
    *y = z;
    return ;
}

// prints out int list
void display(int a[], int length) {
    int i;
    printf("[");

    for(i = 0;i<=length;i++) {
      printf("%d ",a[i]);
  }

  printf("]\n");
}

// prints out float list
void fdisplay(double a[], int length) {
    int i;
    printf("[");

    for(i = 0;i<=length;i++) {
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

double fmedian_of_3(double a[], int left, int right)
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

void fquickSort(double a[], int left, int right)
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

int partition(int a[], int left, int right, int length)
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
        display(a, length);
      }
    }
  }
  swap(&a[i+1], &a[right]);
  if (a[i+1] != a[right] && ((i+1) != right)) {
    printf("numbers swapped: %d,%d\n", a[i+1],a[right]);
    printf("array after swap: ");
    display(a, length);
  }
  return i+1;
}

double fpartition(double a[], int left, int right, int length)
{
  double x = a[right];
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
        fdisplay(a, length);
      }
    }
  }
  fswap(&a[i+1], &a[right]);
  if (a[i+1] != a[right] && ((i+1) != right)) {
    printf("numbers swapped: %f,%f\n", a[i+1],a[right]);
    printf("array after swap: ");
    fdisplay(a, length);
  }
  return i+1;
}

int show_median_of_3(int a[], int left, int right, int length)
{
    // get middle of array
    int middle = (left + right)/2;
    printf("=====================================================================================\n");
    printf("At this step:\n");
    printf("current array:");
    display(a, length);

    // rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        swap(&a[left],&a[middle]);
        printf("pivot swapped: %d,%d\n", a[middle],a[left]);
        printf("array after swap: ");
        display(a, length);
    }
    if (a[right] < a[left]) {
        swap(&a[left],&a[right]);
        printf("pivot swapped: %d,%d\n", a[right],a[left]);
        printf("array after swap: ");
        display(a, length);
    }
    if (a[right]< a[middle]) {
        swap(&a[middle],&a[right]);
        printf("pivot swapped: %d,%d\n", a[right],a[middle]);
        printf("array after swap: ");
        display(a, length);
    }

    swap(&a[middle], &a[right - 1]);
    if (a[middle] != a[right-1] && (middle != (right-1))) {
      printf("numbers swapped: %d,%d\n", a[middle],a[right-1]);
      printf("array after swap: ");
      display(a, length);
    }
    printf("pivot is %d\n", a[right-1]);
    return a[right - 1];
}

double show_fmedian_of_3(double a[], int left, int right, int length)
{
// get middle of array
    int middle = (left + right)/2;
    printf("=====================================================================================\n");
    printf("At this step:\n");
    printf("current array:");
    fdisplay(a, length);

// rearrange arrange now with pivot known
    if (a[middle] < a[left]) {
        fswap(&a[left],&a[middle]);
        printf("pivot swapped: %f,%f\n", a[middle],a[left]);
        printf("array after swap: ");
        fdisplay(a, length);
    }
    if (a[right] < a[left]) {
        fswap(&a[left],&a[right]);
        printf("pivot swapped: %f,%f\n", a[right],a[left]);
        printf("array after swap: ");
        fdisplay(a, length);
    }
    if (a[right] < a[middle]) {
        fswap(&a[middle],&a[right]);
        printf("pivot swapped: %f,%f\n", a[right],a[middle]);
        printf("array after swap: ");
        fdisplay(a, length);
    }

    fswap(&a[middle], &a[right - 1]);
    if (a[middle] != a[right-1] && (middle != (right-1))) {
      printf("numbers swapped: %f,%f\n", a[middle],a[right-1]);
      printf("array after swap: ");
      fdisplay(a, length);
    }
    printf("pivot is %f\n", a[right-1]);
    return a[right - 1];
}

void showQuickSort(int a[], int left, int right, int length)
{
  if(left < right)
  {
    show_median_of_3(a, left, right, length);

    int q = partition(a, left, right, length);
    showQuickSort(a, left, q-1, length);
    showQuickSort(a, q+1, right, length);
  }
}

void showfQuickSort(double a[], int left, int right, int length)
{
  if(left < right)
  {
    show_fmedian_of_3(a, left, right, length);

    int q = fpartition(a, left, right, length);
    showfQuickSort(a, left, q-1, length);
    showfQuickSort(a, q+1, right, length);
  }
}

void cShowQuickSort(int a[], int length) {
  int right = length - 1;
  length = length - 1;
  showQuickSort(a, 0, right, length);
}

void cShowfQuickSort(double a[], int length) {
  int right = length - 1;
  length = length - 1;
  showfQuickSort(a, 0, right, length);
}

void cQuickSort(int a[], int length) {
  length = length - 1;
  quickSort(a, 0, length);
}

void cQuickfSort(double a[], int length) {
  length = length - 1;
  fquickSort(a, 0, length);
}
//
// int main()
// {
//     int a[] = {10, 35, 25, 56, 2002, 100, 90, 86, 40, 50, 7};
//     cShowQuickSort(a, 11);
//     cQuickSort(a, 11);
//
//     int b[] = {85,331,234,46,4,3,22,89,7,12, 33, 44,55,66,77};
//     cShowQuickSort(b, 15);
//     cQuickSort(b, 15);
//
//     double c[] = { 3.4, 56.2, 5.3, 6.0, 2.6, 566.7, 778.3};
//     cShowfQuickSort(c, 7);
//     cQuickfSort(c, 7);
//
//     double d[] = { 55.4, 36.2, 23.3, 16.0, 52.6, 26.7, 78.3};
//     cShowfQuickSort(d, 7);
//     cQuickfSort(d, 7);
//
//     int f[] = {10, 100, 30, 90, 40, 50, 70};
//     cShowQuickSort(f, 7);
//     cQuickSort(f, 7);
//
//     int g[] = {4,6,3,2,1,9,7};
//     cShowQuickSort(g, 7);
//     cQuickSort(g, 7);
//
// }
