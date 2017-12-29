exports.config = {
  title: 'Counter Strike',
  public_path: process.env.NODE_ENV === 'production'
               ? '/dist/'
               : 'http://localhost:8080/dist/'
}
