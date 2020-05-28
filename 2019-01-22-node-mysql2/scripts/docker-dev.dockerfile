FROM mysql:5.7

ENV MYSQL_DATABASE=foo
ENV MYSQL_ROOT_PASSWORD=this_is_mysql_root_password

ADD ./scripts/init.sql /docker-entrypoint-initdb.d

EXPOSE 3306
