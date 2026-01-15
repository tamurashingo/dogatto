# dogatto

A tag-based TODO management web application built with [clails](https://github.com/tamurashingo/clails) framework.

## Architecture

dogatto is a **Full SPA (Single Page Application)** built with:
- **Backend**: Common Lisp (clails framework) + MySQL + Redis
- **Frontend**: React + TypeScript + Vite + React Router

### Key Features

- **Client-Side Routing**: React Router handles all page transitions in the browser
- **REST API**: Backend provides JSON APIs under `/api/*`
- **Universal HTML**: Server returns the same HTML for all non-API routes
- **Fast Navigation**: No server requests needed for page transitions

See [Architecture Documentation](docs/architecture.md) for detailed information about the routing flow and design decisions.

## Prerequisites

- Docker
- Docker Compose

## Documentation

- [Architecture](docs/architecture.md) - Full SPA architecture and routing design
- [Contributing Guide](CONTRIBUTING.md) - Development workflow and guidelines
- [Environment Variables](docs/environment.md) - Configuration options
- [Database Schema](docs/database.md) - Database structure and design
- [API Conventions](docs/api-conventions.md) - REST API standards
- [Troubleshooting](docs/troubleshooting.md) - Common issues and solutions

## Getting Started

### 1. Build Docker Image

```bash
make build
```

### 2. Start Development Environment

```bash
make up
```

This will start:
- Application server on http://localhost:5000
- Swank server on localhost:4005 (for REPL development)
- MySQL database on localhost:3306
- Redis on localhost:6379

### 3. Setup Database

Create the database:

```bash
make db.create
```

Run migrations:

```bash
make db.migrate
```

Seed the database (optional):

```bash
make db.seed
```

### 4. Build Frontend

Build the React frontend:

```bash
make front-build
```

For development with hot reload:

```bash
make front-dev
```

This will start the Vite development server on http://localhost:3000

**Note**: In development mode (`CLAILS_ENV=develop` or `test`), the backend server automatically references the Vite development server for frontend assets. The `VITE_DEV_SERVER_URL` environment variable (default: `http://localhost:3000`) controls this behavior.

### 5. Access the Application

Open your browser and navigate to:
```
http://localhost:5000
```

The backend serves HTML that loads JavaScript from the Vite dev server (port 3000) in development mode.

## Development

### REPL Development with Swank

Connect to the Swank server from your editor (Emacs/SLIME, Vim/Slimv, etc.):

- Host: `localhost`
- Port: `4005`

### Frontend Development

The frontend is built with React + TypeScript + Vite and uses React Router for client-side routing.

Source code is located in `front/src/`:
- `router.tsx` - React Router configuration (route definitions)
- `api/` - API client and error handling
- `components/` - React components
- `contexts/` - React contexts (e.g., AuthContext)
- `hooks/` - Custom React hooks
- `pages/` - Page components
- `types/` - TypeScript type definitions

Build output goes to `public/assets/`.

**Routing Architecture:**
- All page transitions happen in the browser (no server requests)
- Backend serves the same HTML for all non-API routes
- React Router maps URLs to page components
- See [Architecture Documentation](docs/architecture.md) for details

### Available Make Commands

| Command | Description |
|---------|-------------|
| `make build` | Build Docker image |
| `make rebuild` | Rebuild Docker image without cache |
| `make up` | Start containers in detached mode |
| `make down` | Stop and remove containers |
| `make console` | Open bash shell in the application container |
| `make logs` | View application logs |
| `make logs.mysql` | View MySQL logs |
| `make logs.redis` | View Redis logs |
| `make db.create` | Create database |
| `make db.migrate` | Run pending migrations |
| `make db.rollback` | Rollback the last migration |
| `make db.seed` | Load seed data |
| `make db.test.create` | Create test database |
| `make db.test.migrate` | Run migrations for test database |
| `make test` | Run tests |
| `make front-dev` | Start frontend dev server with hot reload |
| `make front-build` | Build frontend for production |
| `make front-preview` | Preview production build |

### Project Structure

```
.
├── app/
│   ├── config/         # Configuration files
│   ├── controllers/    # Controller files
│   ├── models/         # Model files
│   └── views/          # View templates
├── db/
│   ├── migrate/        # Database migration files
│   └── seeds.lisp      # Seed data
├── docker/
│   ├── clails/
│   │   └── Dockerfile  # Clails application Dockerfile
│   ├── mysql/          # MySQL configuration
│   ├── redis/          # Redis configuration
│   └── run-dev.sh      # Application startup script
├── docker-compose.yml  # Docker Compose configuration
├── .env.example        # Environment variables template
├── .env.test           # Test environment variables
├── front/              # Frontend React application
│   ├── src/
│   │   ├── api/        # API client
│   │   ├── components/ # React components
│   │   ├── contexts/   # React contexts
│   │   ├── hooks/      # Custom hooks
│   │   ├── pages/      # Page components
│   │   └── types/      # TypeScript types
│   ├── package.json    # Frontend dependencies
│   └── vite.config.ts  # Vite configuration
├── public/             # Static assets
├── test/               # Test files
└── Makefile            # Make commands
```

## Database Configuration

**MySQL Configuration**

Default settings:
- Host: `mysql` (inside Docker), `localhost` (from host)
- Port: `3306`
- Username: `dogatto`
- Password: `password`
- Database: `dogatto_development`

**Redis Configuration**

Default settings:
- Host: `redis` (inside Docker), `localhost` (from host)
- Port: `6379`

You can override these settings using environment variables in docker-compose.yml.

## Testing

### Running Tests

Before running tests for the first time, set up the test database:

```bash
make db.test.create
make db.test.migrate
```

Run all tests:

```bash
make test
```

Tests run in a separate test environment (`CLAILS_ENV=test`) with its own database (`dogatto_test`). This ensures that test data doesn't interfere with development data.

The test configuration is located in `.env.test`.

### Continuous Integration

Tests are automatically run on GitHub Actions for:
- Pushes to `main`, `develop`, and feature branches
- Pull requests to `main` and `develop`

The CI workflow automatically sets up the test database and runs migrations before executing tests.

## Troubleshooting

### Container won't start

Check logs:
```bash
make logs
```

### Database connection issues

Make sure the database container is running:
```bash
docker compose ps
```

Check database logs:
```bash
make logs.mysql
```

### Redis connection issues

Check Redis logs:
```bash
make logs.redis
```

### Reset everything

Stop containers and remove volumes:
```bash
make down
docker volume rm dogatto-mysql-data dogatto-redis-data
```

## License

[Add your license here]
