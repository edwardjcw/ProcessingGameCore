import './App.css'
import { Game } from './components/Game'
import { DndProvider } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'

function App() {
  return (
    <DndProvider backend={HTML5Backend}>
      <h1>Processing Game</h1>
      <Game />
    </DndProvider>
  )
}

export default App
