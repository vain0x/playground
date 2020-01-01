void inc(void (*inc_ret)(int), int* x) {
    int KAddPrim = x + 1;
    int KAssignPrim = x = KAddPrim;
    inc_ret(KAssignPrim);
}

void inc_ret_3(int* inc_res_2, void (*main_ret)(int)) {
    main_ret(inc_res_2);
}

void inc_ret_2(int* inc_res, int* a, void (*main_ret)(int)) {
    inc(inc_ret_3, &a);
}

void a_next(int a, void (*main_ret)(int)) {
    inc(inc_ret_2, &a);
}

void main(void (*main_ret)(int)) {
    a_next(1, &main_ret);
}
