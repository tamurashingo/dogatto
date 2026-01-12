# Troubleshooting Guide

This guide helps you diagnose and fix common issues with DOGATTO.

## Table of Contents

- [Docker Issues](#docker-issues)
- [Database Issues](#database-issues)
- [Frontend Issues](#frontend-issues)
- [Backend Issues](#backend-issues)
- [Network Issues](#network-issues)

## Docker Issues

### Container Won't Start

**Symptoms:**
- `docker compose up` fails
- Container exits immediately

**Solutions:**

1. Check container logs:
```bash
make logs
docker compose logs -f dogatto-app
```

2. Check all container statuses:
```bash
docker compose ps
```

3. Verify Docker is running:
```bash
docker version
```

4. Check for port conflicts:
```bash
# Linux/Mac
lsof -i :5000
lsof -i :3306
lsof -i :6379

# Windows
netstat -ano | findstr :5000
```

5. Remove and rebuild:
```bash
make down
docker volume prune
make rebuild
make up
```

### Out of Disk Space

**Symptoms:**
- "no space left on device" error

**Solutions:**

1. Clean up Docker:
```bash
docker system prune -a --volumes
```

2. Remove unused images:
```bash
docker image prune -a
```

3. Check disk usage:
```bash
docker system df
```

### Permission Denied Errors

**Symptoms:**
- Files generated inside containers are owned by root
- Cannot modify files

**Solutions:**

1. Fix ownership:
```bash
sudo chown -R $USER:$USER .
```

2. On Linux, ensure your user is in docker group:
```bash
sudo usermod -aG docker $USER
# Log out and back in
```

## Database Issues

### Cannot Connect to Database

**Symptoms:**
- Application fails to start with database connection error
- "Connection refused" or "Unknown host"

**Solutions:**

1. Check MySQL is running:
```bash
docker compose ps mysql
```

2. Check MySQL logs:
```bash
make logs.mysql
```

3. Verify environment variables:
```bash
docker compose config | grep -A 5 CLAILS_DB
```

4. Test connection from host:
```bash
mysql -h 127.0.0.1 -P 3306 -u dogatto -ppassword dogatto_development
```

5. Test connection from app container:
```bash
docker compose exec dogatto-app mysql -h mysql -u dogatto -ppassword dogatto_development
```

6. Ensure MySQL is healthy:
```bash
docker compose ps
# Should show "healthy" status
```

### Migration Fails

**Symptoms:**
- `make db.migrate` fails with error

**Solutions:**

1. Check migration logs:
```bash
make db.migrate
```

2. Verify database exists:
```bash
make db.create
```

3. Check for syntax errors in migration files:
```bash
ls -la db/migrate/
```

4. Rollback last migration and retry:
```bash
make db.rollback
make db.migrate
```

5. Manual migration check:
```bash
docker compose exec dogatto-app clails db:migrate
```

### Data Lost After Restart

**Symptoms:**
- Database is empty after container restart

**Solutions:**

1. Check if volume exists:
```bash
docker volume ls | grep mysql
```

2. Don't use `docker compose down -v` (removes volumes)

3. Use named volumes (already configured in docker-compose.yml)

4. Backup regularly:
```bash
docker compose exec mysql mysqldump -u dogatto -ppassword dogatto_development > backup.sql
```

## Frontend Issues

### Build Fails

**Symptoms:**
- `make front-build` fails
- TypeScript compilation errors

**Solutions:**

1. Check Node.js version:
```bash
node --version
# Should be 18+
```

2. Clean and reinstall dependencies:
```bash
cd front
rm -rf node_modules package-lock.json
npm install
```

3. Check for TypeScript errors:
```bash
cd front
npm run build
```

4. Clear Vite cache:
```bash
cd front
rm -rf node_modules/.vite
```

### Assets Not Loading

**Symptoms:**
- White screen
- 404 errors for JavaScript/CSS files
- Console errors about missing files

**Solutions:**

1. Rebuild frontend:
```bash
make front-build
```

2. Check assets exist:
```bash
ls -la public/assets/
```

3. Verify manifest.json:
```bash
cat public/assets/.vite/manifest.json
```

4. Check asset paths in HTML:
```bash
curl http://localhost:5000/ | grep -E "(script|link)"
```

5. Restart backend to reload manifest:
```bash
docker compose restart dogatto-app
```

### Hot Reload Not Working

**Symptoms:**
- Changes not reflected in browser
- Need to manually refresh

**Solutions:**

1. Check dev server is running:
```bash
make front-dev
```

2. Access dev server directly:
```
http://localhost:3000
```

3. Check Vite config:
```bash
cat front/vite.config.ts
```

4. Clear browser cache (Ctrl+Shift+R or Cmd+Shift+R)

## Backend Issues

### REPL Won't Connect

**Symptoms:**
- Cannot connect to Swank on port 4005

**Solutions:**

1. Check container is running:
```bash
docker compose ps dogatto-app
```

2. Check port is exposed:
```bash
docker compose ps | grep 4005
```

3. Test port is open:
```bash
telnet localhost 4005
```

4. Check application logs:
```bash
make logs | grep -i swank
```

5. Restart container:
```bash
docker compose restart dogatto-app
```

### Internal Server Error

**Symptoms:**
- HTTP 500 errors
- "Internal Server Error" message

**Solutions:**

1. Check application logs:
```bash
make logs
```

2. Look for Common Lisp errors:
```bash
docker compose logs dogatto-app | grep -i error
```

3. Check for package loading errors:
```bash
docker compose logs dogatto-app | grep -i "package"
```

4. Restart application:
```bash
docker compose restart dogatto-app
```

5. Check file permissions:
```bash
ls -la app/
```

### Compilation Errors

**Symptoms:**
- ".fasl" errors
- "undefined function" errors

**Solutions:**

1. Clear Common Lisp cache:
```bash
docker compose exec dogatto-app rm -rf ~/.cache/common-lisp/
```

2. Restart container:
```bash
docker compose restart dogatto-app
```

3. Check for syntax errors in Lisp files

4. Verify package imports:
```bash
grep -r "import-from" app/
```

## Network Issues

### Cannot Access Application

**Symptoms:**
- Cannot open http://localhost:5000
- Connection refused

**Solutions:**

1. Check application is running:
```bash
docker compose ps
curl -I http://localhost:5000/health
```

2. Check firewall:
```bash
# Linux
sudo ufw status
sudo ufw allow 5000

# Mac
# Check System Preferences > Security & Privacy > Firewall
```

3. Try 127.0.0.1 instead of localhost:
```
http://127.0.0.1:5000
```

4. Check port binding:
```bash
docker compose ps
# Should show 0.0.0.0:5000->5000/tcp
```

### Redis Connection Issues

**Symptoms:**
- Redis connection errors in logs

**Solutions:**

1. Check Redis is running:
```bash
docker compose ps redis
make logs.redis
```

2. Test Redis connection:
```bash
docker compose exec redis redis-cli ping
# Should return: PONG
```

3. Check from app container:
```bash
docker compose exec dogatto-app redis-cli -h redis ping
```

4. Verify Redis configuration:
```bash
docker compose exec redis cat /usr/local/etc/redis/redis.conf
```

## Performance Issues

### Slow Database Queries

**Solutions:**

1. Check slow query log:
```bash
docker compose exec mysql mysql -u root -prootpassword -e "SHOW VARIABLES LIKE 'slow_query_log%';"
```

2. Analyze query performance:
```sql
EXPLAIN SELECT ...;
```

3. Check indexes:
```sql
SHOW INDEX FROM todos;
```

4. Monitor database:
```bash
docker compose exec mysql mysqladmin -u root -prootpassword status
```

### High Memory Usage

**Solutions:**

1. Check container stats:
```bash
docker stats
```

2. Limit container resources in docker-compose.yml:
```yaml
services:
  dogatto-app:
    deploy:
      resources:
        limits:
          memory: 512M
```

3. Check for memory leaks in application logs

## Getting Help

If you can't resolve the issue:

1. **Check logs first:**
```bash
make logs
make logs.mysql
make logs.redis
```

2. **Gather system information:**
```bash
docker version
docker compose version
node --version
```

3. **Check for known issues** in the repository's issue tracker

4. **Create a new issue** with:
   - Clear description of the problem
   - Steps to reproduce
   - Relevant log output
   - System information
   - What you've already tried

## Common Commands Cheat Sheet

```bash
# View logs
make logs              # Application logs
make logs.mysql        # MySQL logs
make logs.redis        # Redis logs

# Container management
make up                # Start containers
make down              # Stop containers
make restart           # Restart containers
docker compose ps      # Check status

# Database
make db.migrate        # Run migrations
make db.rollback       # Rollback migration
make db.create         # Create database

# Frontend
make front-build       # Build frontend
make front-dev         # Start dev server

# Debugging
make console           # Enter container
docker compose exec dogatto-app bash
docker compose logs -f dogatto-app --tail 100
```
