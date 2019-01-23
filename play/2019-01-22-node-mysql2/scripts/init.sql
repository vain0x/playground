use foo;

create table employees(
    employee_id bigint primary key,
    employee_name varchar(255) not null
);

insert into employees(
    employee_id,
    employee_name
) values
    (1, 'John Doe'),
    (2, 'Jane Doe');
