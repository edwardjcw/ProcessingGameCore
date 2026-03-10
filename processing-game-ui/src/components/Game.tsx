import { useState, useEffect } from 'react';
import type { Environment } from '../types';
import { Program } from './Program';
import { Processor } from './Processor';

export const Game = () => {
    const [environment, setEnvironment] = useState<Environment | null>(null);
    const [error, setError] = useState<string | null>(null);
    const [tickAmount, setTickAmount] = useState<number>(1);

    const fetchEnvironment = async () => {
        setError(null);
        try {
            const response = await fetch('http://localhost:5250/api/game');
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const data = await response.json();
            setEnvironment(data);
        } catch (e) {
            if (e instanceof Error) {
                setError(e.message);
            } else {
                setError("An unknown error occurred");
            }
            console.error('Error fetching environment:', e);
        }
    };

    useEffect(() => {
        fetchEnvironment();
    }, []);

    const handleDrop = async (adoId: string, processorId: string) => {
        try {
            const response = await fetch('http://localhost:5250/api/game/move', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ adoId, processorId }),
            });
            if(response.ok) {
                const data = await response.json();
                setEnvironment(data);
            } else {
                const errorText = await response.text();
                alert(`Error moving ado: ${errorText}`);
                fetchEnvironment(); // to get back to consistent state
            }
        } catch (error) {
            console.error('Error moving ado:', error);
        }
    };

    const handleTick = async (amount: number = 1) => {
        try {
            const response = await fetch('http://localhost:5250/api/game/tick', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ amount }),
            });
            if (response.ok) {
                const data = await response.json();
                setEnvironment(data);
            } else {
                const errorText = await response.text();
                alert(`Error advancing time: ${errorText}`);
            }
        } catch (error) {
            console.error('Error ticking:', error);
        }
    };

    if (error) {
        return <div>Error: {error}. Please ensure the backend server is running and accessible.</div>;
    }

    if (!environment) {
        return <div>Loading...</div>;
    }

    return (
        <div>
            <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
                <h2>Ticks: {environment.ticks}</h2>
                <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
                    <input 
                        type="number" 
                        value={tickAmount} 
                        onChange={(e) => setTickAmount(Math.max(1, parseInt(e.target.value) || 1))} 
                        style={{ width: '60px', padding: '5px' }}
                        min="1"
                    />
                    <button onClick={() => handleTick(tickAmount)} style={{ padding: '5px 15px', cursor: 'pointer' }}>Advance Ticks</button>
                </div>
            </div>
            <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                <div>
                    <h3>Programs</h3>
                    {Object.values(environment.programs).map((program) => (
                        <Program key={program.id} program={program} handleDrop={handleDrop} />
                    ))}
                </div>
                <div>
                    <h3>Processors</h3>
                    {Object.values(environment.processors).map((processor) => (
                        <Processor key={processor.id} processor={processor} />
                    ))}
                </div>
            </div>
        </div>
    );
};
