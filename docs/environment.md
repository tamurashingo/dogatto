# Environment Variables

This document describes the environment variables used in DOGATTO.

## Docker Compose Environment Variables

All environment variables are defined in `docker-compose.yml` with sensible defaults.

### Application Server

| Variable | Default | Description |
|----------|---------|-------------|
| `CLAILS_HOME` | `/app` | Application root directory |
| `BIND_ADDRESS` | `0.0.0.0` | Server bind address |
| `PORT` | `5000` | Application server port |
| `SWANK_ADDRESS` | `0.0.0.0` | Swank REPL server bind address |
| `SWANK_PORT` | `4005` | Swank REPL server port |

### Database (MySQL)

| Variable | Default | Description |
|----------|---------|-------------|
| `CLAILS_DB_HOST` | `mysql` | MySQL host (use `localhost` from host machine) |
| `CLAILS_DB_PORT` | `3306` | MySQL port |
| `CLAILS_DB_USERNAME` | `dogatto` | MySQL username |
| `CLAILS_DB_PASSWORD` | `password` | MySQL password |
| `CLAILS_DB_NAME` | `dogatto_development` | Database name |
| `MYSQL_ROOT_PASSWORD` | `rootpassword` | MySQL root password |

### Redis

| Variable | Default | Description |
|----------|---------|-------------|
| `REDIS_HOST` | `redis` | Redis host (use `localhost` from host machine) |
| `REDIS_PORT` | `6379` | Redis port |

## Overriding Environment Variables

### Using .env File

Create a `.env` file in the project root (this file is ignored by git):

```bash
# .env
PORT=8080
CLAILS_DB_PASSWORD=my-secure-password
REDIS_PORT=6380
```

Docker Compose will automatically load these variables and override defaults.

### Using Export

You can also export environment variables before running docker-compose:

```bash
export PORT=8080
export CLAILS_DB_PASSWORD=my-secure-password
docker compose up
```

## Development vs Production

### Development Environment

The default configuration is optimized for development:
- Debug logging enabled
- REPL server accessible
- Hot reload for frontend
- Less restrictive security

### Production Environment (Future)

For production deployment, you should:
- Change all default passwords
- Use strong, randomly generated secrets
- Set `BIND_ADDRESS` and `SWANK_ADDRESS` to `127.0.0.1` or remove Swank entirely
- Enable HTTPS
- Use environment-specific database credentials
- Configure proper backup strategies

## Security Considerations

⚠️ **Important Security Notes:**

1. **Never commit .env files** with real credentials to version control
2. **Change default passwords** before deploying to production
3. **Use secrets management** for production environments
4. **Limit database user permissions** to only what's needed
5. **Use SSL/TLS** for database connections in production

## Example Configurations

### Minimal Development Setup

```bash
# .env
# Uses all defaults - suitable for local development
```

### Custom Ports

```bash
# .env
PORT=3000
MYSQL_PORT=3307
REDIS_PORT=6380
SWANK_PORT=4006
```

### Production-like Setup

```bash
# .env.production
PORT=80
BIND_ADDRESS=0.0.0.0
CLAILS_DB_HOST=production-db.example.com
CLAILS_DB_USERNAME=dogatto_prod
CLAILS_DB_PASSWORD=<strong-password>
CLAILS_DB_NAME=dogatto_production
REDIS_HOST=production-redis.example.com
# Don't expose Swank in production
SWANK_PORT=
```

## Accessing Services from Host Machine

### From Application Container

Use service names defined in docker-compose.yml:
- Database: `mysql:3306`
- Redis: `redis:6379`

### From Host Machine

Use localhost with mapped ports:
- Application: `http://localhost:5000`
- Swank REPL: `localhost:4005`
- MySQL: `localhost:3306`
- Redis: `localhost:6379`

## Troubleshooting

### Connection Refused

If you get connection refused errors:

1. Check that the service is running:
   ```bash
   docker compose ps
   ```

2. Verify port mappings:
   ```bash
   docker compose ps
   ```

3. Check service logs:
   ```bash
   make logs
   make logs.mysql
   make logs.redis
   ```

### Environment Variables Not Applied

1. Restart containers after changing .env:
   ```bash
   docker compose down
   docker compose up -d
   ```

2. Verify variables are loaded:
   ```bash
   docker compose config
   ```

### Database Connection Issues

Check that `CLAILS_DB_HOST` matches the service name:
- Inside containers: use service name (`mysql`)
- From host: use `localhost`

## References

- [Docker Compose Environment Variables](https://docs.docker.com/compose/environment-variables/)
- [MySQL Docker Documentation](https://hub.docker.com/_/mysql)
- [Redis Docker Documentation](https://hub.docker.com/_/redis)
