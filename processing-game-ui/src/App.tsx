import { useState, useEffect } from 'react';
import type { Environment } from './types';
import { Game } from './components/Game';
import { NewGame } from './components/NewGame';
import { DndProvider } from 'react-dnd';
import { HTML5Backend } from 'react-dnd-html5-backend';
import { Container, Typography } from '@mui/material';
import { api } from './api';

function App() {
    const [environment, setEnvironment] = useState<Environment | null>(null);

    const fetchEnvironment = async () => {
        try {
            const data = await api.getEnvironment();
            setEnvironment(data);
        } catch (e) {
            console.error('Error fetching environment:', e);
        }
    };

    const handleNewGame = async (programs: number, processors: number, ados: number) => {
        try {
            const data = await api.newGame({ programs, processors, ados });
            setEnvironment(data);
        } catch (error) {
            console.error('Error creating new game:', error);
            alert(`Error creating new game: ${error}`);
        }
    };

    return (
        <DndProvider backend={HTML5Backend}>
            <Container>
                <Typography variant="h2" component="h1" gutterBottom>
                    Processing Game
                </Typography>
                {environment ? (
                    <Game environment={environment} setEnvironment={setEnvironment} fetchEnvironment={fetchEnvironment} />
                ) : (
                    <NewGame onNewGame={handleNewGame} />
                )}
            </Container>
        </DndProvider>
    );
}

export default App;
