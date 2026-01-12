# Contributing to DOGATTO

Thank you for your interest in contributing to DOGATTO! This document provides guidelines for contributing to the project.

## Development Setup

### Prerequisites

- Docker & Docker Compose
- Git
- Node.js 18+ (for frontend development)

### Getting Started

1. Clone the repository:
```bash
git clone <repository-url>
cd dogatto
```

2. Build and start the development environment:
```bash
make build
make up
```

3. Setup the database:
```bash
make db.create
make db.migrate
```

4. Build the frontend:
```bash
make front-build
```

5. Access the application at http://localhost:5000

## Development Workflow

### Backend Development

The backend is built with Common Lisp using the [clails](https://github.com/tamurashingo/clails) framework.

#### Creating Controllers

Use clails generators:
```bash
clails generate:controller <controller-name>
```

#### Creating Models

```bash
clails generate:model <model-name>
```

This will generate both the model and migration files.

#### Database Migrations

Create a migration:
```bash
clails generate:migration <migration-name>
```

Run migrations:
```bash
make db.migrate
```

Rollback last migration:
```bash
make db.rollback
```

#### REPL Development

Connect to the Swank server for interactive development:
- Host: `localhost`
- Port: `4005`

### Frontend Development

The frontend is built with React, TypeScript, and Vite.

#### Development Server

Start the development server with hot reload:
```bash
make front-dev
```

Access at http://localhost:3000

#### Building for Production

```bash
make front-build
```

Built assets will be placed in `public/assets/`.

#### Adding Dependencies

```bash
cd front
npm install <package-name>
```

## Code Style Guidelines

### Backend (Common Lisp)

- Use `#:` notation for package keywords
- Don't use `use` in package definitions (except for `:cl`)
- Use `import-from` for external symbols
- Class names should be wrapped in `< >`: `<user>`, `<todo>`
- Database column names use kebab-case: `is-active`, `created-at`
- Write comments and docstrings in English
- Follow docstring format specified in AGENTS.md

Example docstring:
```lisp
(defun create-user (username email)
  "Creates a new user with the given credentials.

   @param username [string] The username
   @param email [string] The email address
   @return [<user>] The created user object
   @condition validation-error When credentials are invalid
   "
  ...)
```

### Frontend (TypeScript/React)

- Write comments and JSDoc in English
- Use TypeScript strict mode
- Follow JSDoc format specified in AGENTS.md
- Use functional components with hooks
- Use proper type definitions (no `any`)

Example JSDoc:
```typescript
/**
 * Creates a new user account.
 *
 * @param username [string] The username
 * @param email [string] The email address
 * @return [Promise<User>] The created user
 * @throws [ApiError] When validation fails
 */
function createUser(username: string, email: string): Promise<User> {
  ...
}
```

## Testing

Run backend tests:
```bash
# TODO: Add test command when available
```

Run frontend tests:
```bash
cd front
npm test
```

## Commit Guidelines

### Commit Message Format

```
<type>: <subject>

<body>

<footer>
```

### Types

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

### Example

```
feat: Add user authentication

- Implement login endpoint
- Add JWT token generation
- Create authentication middleware

Resolves #123
```

## Pull Request Process

1. Create a feature branch from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes following the code style guidelines

3. Test your changes:
   - Backend: Ensure the application starts and works correctly
   - Frontend: Run `npm run build` to verify it compiles

4. Commit your changes with a descriptive commit message

5. Push to your branch:
   ```bash
   git push origin feature/your-feature-name
   ```

6. Create a Pull Request with:
   - Clear description of changes
   - Reference to related issues
   - Screenshots (if UI changes)

## Project Structure

```
.
├── app/                    # Backend application
│   ├── config/            # Configuration files
│   ├── controllers/       # Controllers
│   ├── helpers/           # Helper functions
│   ├── models/            # Models
│   └── views/             # View templates
├── db/                    # Database files
│   └── migrate/           # Migration files
├── docs/                  # Documentation
├── front/                 # Frontend application
│   └── src/
│       ├── api/           # API client
│       ├── components/    # React components
│       ├── contexts/      # React contexts
│       ├── hooks/         # Custom hooks
│       ├── pages/         # Page components
│       └── types/         # TypeScript types
├── public/                # Static assets (generated)
└── test/                  # Test files
```

## Resources

- [clails Documentation](https://github.com/tamurashingo/clails)
- [React Documentation](https://react.dev)
- [TypeScript Documentation](https://www.typescriptlang.org)
- [Vite Documentation](https://vitejs.dev)

## Questions?

If you have questions or need help, please:
1. Check existing documentation
2. Search through existing issues
3. Create a new issue with the `question` label

## License

By contributing, you agree that your contributions will be licensed under the same license as the project.
