import Header from '../components/Header';
import '../styles/todos.css';

/**
 * Todos page component.
 *
 * Main TODO list page (to be implemented).
 */
export default function TodosPage(): React.JSX.Element {
  return (
    <div className="todos-page">
      <Header />
      <main className="main-content">
        <h1>My TODOs</h1>
        <p>TODO list functionality will be implemented here.</p>
      </main>
    </div>
  );
}
