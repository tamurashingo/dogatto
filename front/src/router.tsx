import { createBrowserRouter } from 'react-router-dom';
import App from './App';
import LoginPage from './pages/LoginPage';
import RegisterPage from './pages/RegisterPage';
import TodosPage from './pages/TodosPage';
import TodoCreatePage from './pages/TodoCreatePage';
import TodoDetailPage from './pages/TodoDetailPage';
import TodoEditPage from './pages/TodoEditPage';
import TagsPage from './pages/TagsPage';
import TagDetailPage from './pages/TagDetailPage';
import LabelsPage from './pages/LabelsPage';
import ProtectedRoute from './components/ProtectedRoute';

/**
 * Application router configuration.
 * 
 * Defines all routes for the application including authentication and main pages.
 * 
 * @return {Router} Configured browser router instance
 */
export const router = createBrowserRouter([
  {
    path: '/',
    element: <App />,
  },
  {
    path: '/login',
    element: <LoginPage />,
  },
  {
    path: '/register',
    element: <RegisterPage />,
  },
  {
    path: '/todos',
    element: (
      <ProtectedRoute>
        <TodosPage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/todos/new',
    element: (
      <ProtectedRoute>
        <TodoCreatePage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/todos/:ulid',
    element: (
      <ProtectedRoute>
        <TodoDetailPage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/todos/:ulid/edit',
    element: (
      <ProtectedRoute>
        <TodoEditPage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/tags',
    element: (
      <ProtectedRoute>
        <TagsPage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/tags/:ulid',
    element: (
      <ProtectedRoute>
        <TagDetailPage />
      </ProtectedRoute>
    ),
  },
  {
    path: '/labels',
    element: (
      <ProtectedRoute>
        <LabelsPage />
      </ProtectedRoute>
    ),
  },
]);
