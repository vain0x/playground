import mysql from "mysql2"

const main = async () => {
  const pool = mysql.createPool({
    host: "localhost",
    user: "root",
    password: "this_is_mysql_root_password",
    database: "foo",
    waitForConnections: true,
    connectionLimit: 10,
    queueLimit: 0,
  })

  pool.getConnection((err, connection) => {
    if (err) {
      return console.error(err)
    }

    connection.query(`
      select *
      from employees
    `, [], (err, results, fields) => {
        if (err) {
          console.error(err)
          return
        }

        console.log(results)
        pool.releaseConnection(connection)

        pool.end(err => {
          if (err) {
            return console.error(err)
            console.error("disposed.")
          }
        })
      })
  })
}

main()
